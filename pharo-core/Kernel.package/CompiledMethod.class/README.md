Specific version of compiled code for method's.

CompiledMethod instances can be in two forms:
- up until Pharo 6, they encode both the method and the inner closures bytecodes and literals.
- since Pharo 6, they encode only the method's bytecodes and literals.

In addition the execution mechanics, a compiled method have two extra optional literals. The last literal, optional if there are no super sends, is the class in which the method is installed. The last but one literal is either the method's selector or an AdditionalMethodState instance. AdditionalMethodState instances are used to encode additional state to a method, as for example the pragmas.





