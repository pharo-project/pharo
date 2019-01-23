"
I implement the common behavior of the processing of arguments for 64 bits platforms.
The 64 bits platforms uses the registers to pass parameters.
The integer parameters are passed in the general purpose parameters (the address of the copy of them is in #integerRegisterPointer) and the float parameters are passed in the set of registers XMM0-XMM8. This float point registers are accessible through the #floatRegisterPointer.

As the different calling conventions uses the registers in different ways and also they use different number of registers I only have limited reused code.
"
Class {
	#name : #FFIAbstract64BitsArgumentReader,
	#superclass : #FFICallbackArgumentReader,
	#instVars : [
		'currentRegisterIndex'
	],
	#category : #'UnifiedFFI-Callbacks'
}

{ #category : #accessing }
FFIAbstract64BitsArgumentReader >> floatRegisterPointer [
	
	^ callbackContext floatregargsp
]

{ #category : #initialization }
FFIAbstract64BitsArgumentReader >> initialize [ 
	super initialize.
	currentRegisterIndex := 1.	
]

{ #category : #accessing }
FFIAbstract64BitsArgumentReader >> integerRegisterPointer [
	
	^ callbackContext intregargsp
]

{ #category : #accessing }
FFIAbstract64BitsArgumentReader >> numberOfRegisters [

	^ self subclassResponsibility
]
