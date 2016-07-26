I provide a different view of all variables from  a Context suitable for a bytecode debugger:
- I show the complete stack of variables stored by the context (both named and unnamed variables)
- I show the slots of the receiver object using the notation rcv0, rcv1, etc., as this is how slots are accessed in bytecode instructions