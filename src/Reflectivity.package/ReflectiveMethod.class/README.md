I am a method, but not a compiled method storing bytecode, but a high-level model of a method referencing the AST.

to be compatible to CompiledMethod, I forward sends to a compiled method of myself.

When I am installed in a class, #run:with:in: will compile a new compiledMethod and install it. This compiledMethod has a reference to me. We form a "twin" were we reference each other and either can be installed.

call #invalidate to force the installation of the ReflectiveMethod, and therefore code generation on the next execution.