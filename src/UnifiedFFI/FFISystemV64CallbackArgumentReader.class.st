"
I represent the parsing of arguments in the SystemV 64 bits calling convention.
In this convention the passing of the parameters is done in the registers for the first 6 integers and the first 6 floats.
The count of the register used is indepent of the type. Ex: 

int f(int a, int b, float x, float y, int c)

a -> 1st int register.
b -> 2nd int register.
x -> 1st float register.
y -> 2nd float register.
c -> 3rd int register.

If there are more that 6 parameters of a given type, the seventh goes in the stack.

The structs in this calling convention are passed in the stack or in the registers.
If a struct occupies #maxStructureSizeToPassInRegisters bytes or less it is passed in the registers. Using the two next registers available.
If there are no available registers or the struct is bigger, the stack is used. 

All the integer types are promoted to Int64 or UInt64.
"
Class {
	#name : #FFISystemV64CallbackArgumentReader,
	#superclass : #FFIAbstract64BitsArgumentReader,
	#instVars : [
		'currentFloatRegisterIndex'
	],
	#category : #'UnifiedFFI-Callbacks'
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

{ #category : #'instance creation' }
FFISystemV64CallbackArgumentReader >> maxStructureSizeToPassInRegisters [

	"I am the max size of registers passed in the general purpouse registres.
	The structures that are bigger are passed through the stack"
	^ 16
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
	(type externalTypeSize > self maxStructureSizeToPassInRegisters)
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
