Specific version of compiled code for block's.

CompiledBlock are used only in the recent BlockClosure implementation by Eliot Miranda and Clement Bera, also known as FullBlockClosure. This new representation makes possible heavier runtime optimisations while simplifying a lot of code, both image-side and VM-side.

In addition the execution mechanics, a compiled block have an extra optional literal. The last literal is the compiled code referring to the compiled block. 


