"
When compiling syntactically incorrect code, we compile raising this exception.

This way the debugger opens and the programmer can easily fix the faulty method
"
Class {
	#name : #RuntimeSyntaxError,
	#superclass : #Halt,
	#category : #'Kernel-Exceptions'
}
