Class {
	#name : #FFISystemV64CallbackArgumentReader,
	#superclass : #FFIAbstract64BitsArgumentReader,
	#instVars : [
		'currentFloatRegisterIndex'
	],
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #accessing }
FFISystemV64CallbackArgumentReader >> currentFloatRegisterIndex [
	^ currentFloatRegisterIndex
]

{ #category : #accessing }
FFISystemV64CallbackArgumentReader >> currentIntegerRegisterIndex [
	^ currentRegisterIndex
]

{ #category : #updating }
FFISystemV64CallbackArgumentReader >> increateFloatRegisterIndex [
	
	currentFloatRegisterIndex := currentFloatRegisterIndex + 1
]

{ #category : #updating }
FFISystemV64CallbackArgumentReader >> increateIntegerRegisterIndex [
	
	currentRegisterIndex := currentRegisterIndex + 1
]

{ #category : #initialization }
FFISystemV64CallbackArgumentReader >> initialize [

	super initialize.
	currentFloatRegisterIndex := 1.

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

	baseOffset := (currentRegisterIndex - 1) * Smalltalk wordSize + 1.
	"We have to increase the current register index used"
	currentRegisterIndex := currentRegisterIndex + (type externalTypeSize / Smalltalk wordSize) ceiling.
	 		
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
