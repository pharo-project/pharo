I'm implement a callout mechanism to be used with nb calls formatting.

Callout arguments can be either:
- an integer constant, boolean or nil
- a type name (string or symbol)
- a class name
- a class variable
- any other object, which responds to #asExternalTypeOn:

Options: 
Options may change the execution/compilation of ffi calls. 
Not many are predefined, here a small explanation of them: 

optIndirectCall 		This will perform an "indirect function call" as explained here: https://en.wikipedia.org/wiki/Function_pointer
