I am a copy of the CompilationContext.

It is used

-> in the old Compiler so it is independed from Opal
-> in the new Compiler the class CompilationContext parametrizes Opal to use this Context so it can add instance variables to the context.

The second option is a workaround, we will need a copy of Opal to develop itself *or* need to add transactions to the language ;--)