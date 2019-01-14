Class {
	#name : #FFIAbstract64BitsArgumentReader,
	#superclass : #FFICallbackArgumentReader,
	#instVars : [
		'currentRegisterIndex'
	],
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #accessing }
FFIAbstract64BitsArgumentReader >> floatRegisterPointer [
	
	^ callbackContext floatregargsp
]

{ #category : #accessing }
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
