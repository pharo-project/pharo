I am an SUnit Test of MethodContext and its super type, ContextPart. See also BlockContextTest.
See pages 430-437 of A. Goldberg and D. Robson's  Smalltalk-80 The Language (aka the purple book), which deal with Contexts. My fixtures are from their example. (The Squeak byte codes are not quite the same as Smalltalk-80.)
My fixtures are:
aReceiver         - just some arbitrary object, "Rectangle origin: 100@100 corner: 200@200"
aSender           - just some arbitrary object, thisContext
aCompiledMethod - just some arbitrary method, "Rectangle rightCenter".
aMethodContext   - just some arbitray context ...  

