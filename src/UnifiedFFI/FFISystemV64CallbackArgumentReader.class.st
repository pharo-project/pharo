Class {
	#name : #FFISystemV64CallbackArgumentReader,
	#superclass : #FFICallbackArgumentReader,
	#instVars : [
		'currentFloatRegisterIndex',
		'currentIntegerRegisterIndex'
	],
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #accessing }
FFISystemV64CallbackArgumentReader >> currentFloatRegisterIndex [
	^ currentFloatRegisterIndex
]

{ #category : #accessing }
FFISystemV64CallbackArgumentReader >> currentIntegerRegisterIndex [
	^ currentIntegerRegisterIndex
]

{ #category : #accessing }
FFISystemV64CallbackArgumentReader >> floatRegisterPointer [
	
	^ callbackContext floatregargsp
]

{ #category : #updating }
FFISystemV64CallbackArgumentReader >> increateFloatRegisterIndex [
	
	currentFloatRegisterIndex := currentFloatRegisterIndex + 1
]

{ #category : #updating }
FFISystemV64CallbackArgumentReader >> increateIntegerRegisterIndex [
	
	currentIntegerRegisterIndex := currentIntegerRegisterIndex + 1
]

{ #category : #initialization }
FFISystemV64CallbackArgumentReader >> initialize [

	super initialize.
	currentFloatRegisterIndex := 1.
	currentIntegerRegisterIndex := 1.	
]

{ #category : #accessing }
FFISystemV64CallbackArgumentReader >> integerRegisterPointer [
	
	^ callbackContext intregargsp
]

{ #category : #'calculating-offsets' }
FFISystemV64CallbackArgumentReader >> nextBaseAddressFor: type [
	
	| baseAddressToRead offsetOfBaseAddress registerIndex |
	
	registerIndex := (type isFloatType and: [type isPointer not])  ifTrue: [ self currentFloatRegisterIndex ] ifFalse: [ self currentIntegerRegisterIndex ].
	
	^ registerIndex <= self numberOfRegisters 
		ifTrue:[ 
				"If the parameter is a floating point parameter it comes in the floating point set of registers, not in the general purpose ones."
				(type isFloatType and: [type isPointer not]) 
					ifTrue: [ 
						baseAddressToRead := self floatRegisterPointer.
						self increateFloatRegisterIndex ]
					ifFalse: [
						baseAddressToRead := self integerRegisterPointer.
						self increateIntegerRegisterIndex].
				
				offsetOfBaseAddress := (registerIndex - 1) * Smalltalk wordSize + 1.
				{ baseAddressToRead. offsetOfBaseAddress }] 
		ifFalse:[ super nextBaseAddressFor: type ].


]

{ #category : #'calculating-offsets' }
FFISystemV64CallbackArgumentReader >> nextBaseAddressForStructure: type [

	| baseOffset |

	"If the structure is maller than the max size, it is passed in the general purpouse registers."
	(type externalTypeSize > FFIArchitecture forCurrentArchitecture maxStructureSizeToPassInRegisters)
		ifTrue: [ ^ super nextBaseAddressFor: type ].

	baseOffset := (self currentIntegerRegisterIndex - 1) * 8 + 1.
	"We have to increase the current register index used"
	currentIntegerRegisterIndex := currentIntegerRegisterIndex + (type externalTypeSize / 8) ceiling.
	 		
	^ { self integerRegisterPointer.  baseOffset}
	
]

{ #category : #accessing }
FFISystemV64CallbackArgumentReader >> numberOfRegisters [
	
	^ 6
]

{ #category : #types }
FFISystemV64CallbackArgumentReader >> stackIntegerType [
	
	^ FFIInt64 new
]

{ #category : #types }
FFISystemV64CallbackArgumentReader >> stackUnsignedIntegerType [
	
	^ FFIUInt64 new
]
