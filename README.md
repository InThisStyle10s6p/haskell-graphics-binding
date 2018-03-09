# OpenGL and GLFW bindings

Haskell bindings to OpenGL and GLFW. Very experimental. For instance, I have to
redo the qualified exports, and actually document things. Given the history of
my use of the bindings, there are probably also a lot of bugs and instances of
poor design decisions.

# Some notes on OpenGL and shader writing strategies
The information on the latest OpenGL versions online can be incomplete. I have
noticed that many things (even in the OpenGL 4 reference pages, but especially
in online tutorials, books, and the Khronos OpenGL wiki) are written as though
more recent developments in OpenGL have not taken place, if not outright
suggesting the use of deprecated or removed features of OpenGL. The OpenGL and
GLSL specifications are quite useful in this regard, as are the discussions in
the proposals for language extensions that eventually make it into Core. I am
still new to graphics programming, but here are some of my current practices, as
well as some things that have caused me grief when writing OpenGL applications
using Core 4.5.

- Any mention of targets can be disregarded, except when binding a buffer to an
  indexed location as in the function `glBindBufferRange`. All other functions
  that use them can be replaced by their direct state access equivalents. The
  convention for this seems to be:
  - Any name creation function with *Gen* in it can been replaced by *Create*.
    This eliminates the need to bind a buffer to a target before using it in some
    way. See `glGenBuffers` vs `glCreateBuffers`.
  - Any other function that requires a buffer to be bound to a target can be
    replaced by its *Name* equivalent. See `glBufferSubData` vs
    `glNamedBufferSubData`.
  - The various properties of a vertex array object can be set using the
    `VertexArray` family of functions. See the confusing case of the new
    `glEnableVertexArrayAttrib` vs the old `glEnableVertexAttribArray`.
- Variable size buffers seem to be discouraged. To create buffers with a static
  size, use `glNamedBufferStorage` and update these buffers with
  `glNamedBufferSubData` and `glMapNamedBufferRange`.
- Explicit layout and binding qualifications and the `std140` layout are
  extremely useful. At the moment my only need for these explicit locations are
  in vertex shader input variables, uniform block binding locations, and texture
  sampler unit locations. Note that this seems to eliminate the need to use the
  `glUniformBlockBinding` function, in addition to the various other query
  functions. At least, this practice has done so for me.
- Shader stages can be compiled into separate programs using the
  `glCreateShaderProgramv` function (if you are lazy like me), which can then be
  assembled into program pipelines. Supposedly one has to modify one's shader
  code in some ways to do this. The OpenGL wiki, for example, claims that the
  `gl_PerVertex` variables must be re-declared in the vertex shader. I have not
  found that this makes a difference. It is also good to note here that a
  program that has been set as current with `glUseProgram` will be used instead
  of a pipeline in every instance, so watch out if you mix these two practices.
- Certain defaults in the layout qualifier in GLSL can be set globally in a
  program using the syntax `layout (qualifers here) uniform;`. I use `layout
  (row_major, std140) uniform;` personally.
  
