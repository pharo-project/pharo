I am abstract. Subclasses of me model meta objects for global variables (Class, Global, Pool), called "Literal Variables" in Smalltalk.

I am just a subclass of Association, modeling the binding of the global in either the Smalltalk globals, Undeclared or classPools.


When chaning emit* methods, do not forget to recompile exisiting code:

aGlobal usingMethods do: #recompile 

***NOTE***
When moving binding from Undeclared, we change the class of that binding to either ClassVariable or GlobalVariable.

==> when we use Global subclasses, we will either need to restrict adding variables or add a slow path where we create a new binding and update all users. But this can be done later.