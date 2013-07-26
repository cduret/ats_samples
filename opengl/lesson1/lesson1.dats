staload "prelude/SATS/string.sats"
staload "contrib/GL/SATS/gl.sats"
staload "contrib/GLEXT/SATS/glext.sats"
staload "contrib/GLFW/SATS/glfw.sats"

%{^
#include <GL/gl.h>
#include <GL/glext.h>

void
glBufferData_convert (
  ats_GLenum_type target, ats_GLenum_type type
, ats_size_type tsz, ats_size_type sz
, ats_ptr_type data, ats_GLenum_type usage
) {
  glBufferData (target, sz * tsz, (void *)data, usage);
  return;
} // end of [glBufferData_convert]

void glVertexAttribPointerBuffer (
  ats_GLuint_type indx , ats_GLsizei_type size
, ats_GLenum_type type , ats_GLsizei_type stride
, ats_GLsizeiptr_type pos) {
  glVertexAttribPointer (indx, size, type, GL_FALSE, stride, (void *)pos);
  return;
} // end of [glVertexAttribPointerBuffer]
%}

// maybe/error monad
datatype
maybe_error (a:t@ype+,b:t@ype+) =
  | Error (a, b) of (b) | Just (a, b) of (a)

typedef cloref_maybee_1 (a:t@ype, b: t@ype) = a -<cloref1> maybe_error(a,b)

fun{a,b:t@ype}
return (value: a): maybe_error(a,b) = Just value

fun{a,b:t@ype}
bind (v: maybe_error(a,b), f: cloref_maybee_1(a, b)): maybe_error(a,b) =
  case+ v of
  | Error (_) => v
  | Just (x) => f x

// int, string maybe_error instance
infixl (+) >>=

fun 
>>= (c: maybe_error(int, string), mf: cloref_maybee_1(int,string)): maybe_error(int,string) =
  bind<int,string>(c, mf)

implement bool_of_int(x: int) =
  if x = 0 then false else true

fn compile_log( shader: !GLshader ): void = let
  #define BUFSZ 1024
  var !p_log with pf_log = @[byte][BUFSZ]()
  var len: GLsizei // uninitialized
  prval pf_len = Some_v (view@ (len))
  val () = glGetShaderInfoLog(pf_log, pf_len |shader, GLsizei_of_int BUFSZ, &len, p_log)
  prval Some_v pf = pf_len; prval () = view@ (len) := pf 
  val () = fprint_strbuf (stderr_ref, !p_log);
  val () = prerrf("\n", @())
  prval () = pf_log := bytes_v_of_strbuf_v (pf_log)
in
end

fn display_error(msg: string): void = let
  val v = glGetError()
in
  case+ 0 of
  | _ when v = GL_INVALID_ENUM => prerrf("%s : GL_INVALID_ENUM\n", @(msg))
  | _ when v = GL_INVALID_VALUE => prerrf("%s : GL_INVALID_VALUE\n", @(msg))
  | _ when v = GL_INVALID_OPERATION => prerrf("%s : GL_INVALID_OPERATION\n", @(msg))
  | _ when v = GL_OUT_OF_MEMORY => prerrf("%s : GL_OUT_OF_MEMORY\n", @(msg))
  | _ => ()
end

extern fun 
glVertexAttribPointerBuffer {a:t@ype} (
  indx: GLuint , size: GLsizei , type: GLenum_type a , stride: GLsizei , pos: GLsizeiptr) : void
  = "glVertexAttribPointerBuffer"
// end of [glVertexAttribPointerBuffer]

extern fun 
glBufferDataConvert {n:nat} {a:t@ype} (
  target: GLenum, type: GLenum_type a
, tsz: size_t (sizeof a)
, sz: size_t, data: &(@[a][n]), usage: GLenum
) : void
  = "glBufferData_convert"
// end of [fun]

(* ********** *)

fn setup_glfw(): int = let
  fn glfw_init(): maybe_error(int, string) = let
    val v = glfwInit()
  in
    if ( v = 0 ) then
      Error "Failed to initialize GLFW"
    else Just v
  end
  val glfw_openw_m: cloref_maybee_1(int, string) = lambda where {
    val lambda = lam (c:int): maybe_error(int, string) =<cloref1> let
      val v = glfwOpenWindow(640, 480, 0,0,0,0,0,0,GLFW_WINDOW)
    in
      if ( v = 0 ) then let val () = glfwTerminate() in
        Error("Failed to open GLFW window") end
      else
        Just v
    end
  }
  val setup_opengl_m: cloref_maybee_1(int, string) = lambda where {
    val lambda = lam (c:int): maybe_error(int, string) =<cloref1> let
      val () = glfwSetWindowTitle( "Simple Triangle" )
      val () = glfwEnable( GLFW_STICKY_KEYS )
      val () = glfwSwapInterval( 1 )
      var width: int = 0
      var height: int = 0
      val () = glfwGetWindowSize( &width, &height )
      val () = if ( height <= 0 ) then height := 1
      val () = glViewport (0,0,width,height)
      val () = glClearColor (GLclampf_of_float(0.0f), GLclampf_of_float(0.0f), GLclampf_of_float(0.0f), GLclampf_of_float(0.5f))
      val () = glClearDepth(GLclampd_of_double(1.0))
      val () = glDepthFunc GL_LEQUAL
      val () = glEnable GL_DEPTH_TEST
      val () = glShadeModel GL_SMOOTH
      val () = glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST)
    in
      Just 1
    end
  }
in
  case+ (glfw_init() >>= 
         glfw_openw_m >>= 
         setup_opengl_m) of
  | Error (msg) => let val () = prerrf("ERROR [%s]\n", @(msg)) in 0 end
  | _ => 1
end // of setup_glfw

(* ********** *)

fn init_shaders(vshader_src: string, fshader_src: string): Option @(GLuint, GLuint)  = let
  fn compile_shader(code: string, shader: GLshader): Option GLuint = let
    val (fpf | pstr) = string_takeout_ptr (code)
    val () = glShaderSource__string(shader, pstr)
    prval () = fpf(pstr)
    val () = glCompileShader(shader)
    var shader_ok: GLint
    val () = glGetShaderiv(shader, GL_COMPILE_STATUS, shader_ok)
  in
    if int_of_GLint shader_ok = 0 then let
      val () = compile_log(shader)
      val () = glDeleteShader(shader)
    in None
    end else Some(GLuint_of_GLshader shader)//success
  end
  val vertex_shader = compile_shader(vshader_src, glCreateShader(GL_VERTEX_SHADER))
  val fragment_shader = compile_shader(fshader_src, glCreateShader(GL_FRAGMENT_SHADER))
  val () = display_error("init_shaders")
in
  if option_is_some(vertex_shader) && option_is_some(fragment_shader) then
    Some @(u_vertex_shader, u_fragment_shader) where {
      val Some u_vertex_shader = vertex_shader
      val Some u_fragment_shader = fragment_shader
    }
  else
    None
end // of init_shaders

fn render(program: !GLprogram,
          position_loc: GLuint,
          pmatrix_loc: GLint,
          mvmatrix_loc: GLint,
          p_perspective_mat4: &(@[GLfloat][16]), 
          p_modelview_mat4: &(@[GLfloat][16])): void = let
  // render
  val () = glUseProgram(program)
  val () = glClearColor(GLfloat_of_float 0.0f, GLfloat_of_float 0.0f, GLfloat_of_float 0.0f, GLfloat_of_float 1.0f)
  val () = glClear(GL_COLOR_BUFFER_BIT lor GL_DEPTH_BUFFER_BIT)
  val () = glVertexAttribPointerBuffer(position_loc, GLsizei_of_int 3, GL_FLOAT, GLsizei_of_int 0, GLsizeiptr_of_int1 0)
  val () = glUniformMatrix4fv(pmatrix_loc, GLsizei_of_int1 1, GL_FALSE, p_perspective_mat4)
  val () = glUniformMatrix4fv(mvmatrix_loc, GLsizei_of_int1 1, GL_FALSE, p_modelview_mat4)
  val () = glDrawArrays(GL_TRIANGLES, GLint_of_int 0, GLsizei_of_int 3) 
  val () = glfwSwapBuffers()
  //extern castfn __leak2 (x: GLprogram): void
  //val () = __leak2(program)
in end// end of render()

implement main(argc, argv) = let
  // begin of main
  val a = setup_glfw()
  val vshader = "attribute vec3 vPosition;\n\
                       uniform mat4 uMVMatrix;\n\
                       uniform mat4 uPMatrix;\n\
                       void main(void) {\n\
                         gl_Position = uPMatrix * uMVMatrix * vec4(vPosition, 1.0);\n\
                       }\n"
  val fshader = "precision mediump float;\n\
                     void main() \n\
                     { \n\
                         gl_FragColor = vec4(1.0, 1.0, 1.0, 1.0);\n\
                     } \n"
  var !perspective_mat4 = @[GLfloat](GLfloat_of_float 1.8106601238250732f, GLfloat_of_float 0.0f, GLfloat_of_float 0.0f, GLfloat_of_float 0.0f, 
                                     GLfloat_of_float 0.0f, GLfloat_of_float 2.4142136573791504f, GLfloat_of_float 0.0f, GLfloat_of_float 0.0f,
                                     GLfloat_of_float 0.0f, GLfloat_of_float 0.0f, GLfloat_of_float ~1.0020020008087158f, GLfloat_of_float ~1.0f,
                                     GLfloat_of_float 0.0f, GLfloat_of_float 0.0f, GLfloat_of_float ~0.20020020008087158f, GLfloat_of_float 0.0f)
  var !modelview_mat4 = @[GLfloat](GLfloat_of_float 1.0f, GLfloat_of_float 0.0f, GLfloat_of_float 0.0f, GLfloat_of_float 0.0f,
                                   GLfloat_of_float 0.0f, GLfloat_of_float 1.0f, GLfloat_of_float 0.0f, GLfloat_of_float 0.0f,
                                   GLfloat_of_float 0.0f, GLfloat_of_float 0.0f, GLfloat_of_float 1.0f, GLfloat_of_float 0.0f,
                                   GLfloat_of_float ~1.5f, GLfloat_of_float 0.0f, GLfloat_of_float ~7.0f, GLfloat_of_float 1.0f)
  var !vertices = @[GLfloat](GLfloat_of_float 0.0f, GLfloat_of_float 1.0f, GLfloat_of_float 0.0f, 
                             GLfloat_of_float ~1.0f, GLfloat_of_float ~1.0f, GLfloat_of_float 0.0f, 
                             GLfloat_of_float 1.0f, GLfloat_of_float ~1.0f, GLfloat_of_float 0.0f)
  val b = init_shaders(vshader, fshader)
  val () = if option_is_some b then let
             val Some shaders = b
             val program = glCreateProgram()
             val vertex_shader = GLshader_of_GLuint shaders.0
             val () = glAttachShader(program, vertex_shader )
             val fragment_shader = GLshader_of_GLuint shaders.1
             val () = glAttachShader(program, fragment_shader )
             val () = glLinkProgram(program)
             var program_ok: GLint
             val () = glGetProgramiv(program, GL_LINK_STATUS, program_ok)
             extern castfn __leak1 (x: GLshader): void
             val () = __leak1(vertex_shader)
             val () = __leak1(fragment_shader)
             val () = if int_of_GLint program_ok = 0 then let
                 val () = prerrf("Failed to link shader program\n", @())
                 val () = glDeleteProgram(program)
               in (* program ends here ! *) end
               else let
                 val () = display_error("init_program")
                 // init program
                 val position: GLuint = GLuint_of_uint(uint_of_int(int_of_GLint(
                                          glGetAttribLocation(program, "vPosition"))))
                 val () = glEnableVertexAttribArray(position)
                 val pmatrix = glGetUniformLocation(program, "uPMatrix")
                 val mvmatrix = glGetUniformLocation(program, "uMVMatrix")
                 val () = let
                   var vertex_buffer: GLbuffer
                   val () = glGenBuffer vertex_buffer
                   val () = glBindBuffer(GL_ARRAY_BUFFER, vertex_buffer)

                   val () = glBufferDataConvert(GL_ARRAY_BUFFER, GL_FLOAT, sizeof<GLfloat>, size_of_int 9, !vertices, GL_STATIC_DRAW)
                   val () = display_error("init_buffer")
                   extern castfn __leak3 (x: GLbuffer): void
                 in __leak3(vertex_buffer) end
                 // main loop
                 val () = for( ; ;  ) let
                     val () = render(program, position, pmatrix, mvmatrix, !perspective_mat4, !modelview_mat4)
                     val () = glfwPollEvents()
                   in if ( glfwGetKey(GLFW_KEY_ESC) = GLFW_PRESS ||
                           ~bool_of_int(glfwGetWindowParam(GLFW_OPENED)) ) then 
                     break
                   end

                 extern castfn __leak2 (x: GLprogram): void
                 val () = __leak2(program)
                 
               in glfwTerminate() end
           in end
in end // end of main ..
