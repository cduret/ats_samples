CMAKE_MINIMUM_REQUIRED (VERSION 2.8)
PROJECT (LESSON_GL C)

# add GC support
set (CMAKE_C_FLAGS "-I. -I/usr/local/include -O3 -D_ATS_GCATS")

FIND_PACKAGE (ATS REQUIRED)
IF (NOT ATS_FOUND)
	MESSAGE (FATAL_ERROR ”ATS Not Found!”)
ENDIF ()


# add_library( libpthread STATIC IMPORTED )
# set_target_properties( libpthread PROPERTIES IMPORTED_LOCATION (/usr/lib/) )

ATS_COMPILE (TEST_SRC 
             contrib/GL/SATS/gl.sats
             contrib/GLEXT/SATS/glext.sats
             contrib/GLFW/SATS/glfw.sats
             lesson1.dats)

ADD_EXECUTABLE (lesson1 ${TEST_SRC})
TARGET_LINK_LIBRARIES (lesson1 pthread glfw)
