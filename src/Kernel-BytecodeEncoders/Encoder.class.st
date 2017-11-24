"
I encode names and literals into tree nodes with byte codes for the compiler. Byte codes for literals are not assigned until the tree-sizing pass of the compiler, because only then is it known which literals are actually needed. I also keep track of sourceCode ranges during parsing and code generation so I can provide an inverse map for the debugger.
"
Class {
	#name : #Encoder,
	#superclass : #Object,
	#category : #'Kernel-BytecodeEncoders'
}

{ #category : #'bytecode decoding' }
Encoder class >> quickPrimSpecialConstants [
	"Note: they are different from the encoder special literals."
	^ #(true false nil -1 0 1 2) 
]
