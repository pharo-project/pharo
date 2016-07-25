I'm a FFICallback type. 
I can handle callback type parameters.

What I actually do is to pass the content of #thunk instVar of the objectClass as parameter. 
Thunk needs to be a callback thunk (and then an ExternalAddress).
The C side of a callback is void* (because is a pointer to a function)