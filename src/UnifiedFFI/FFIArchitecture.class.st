"
I'm a base class for different kind of architectures. 
My children implement different architecture-dependent functionality.
"
Class {
	#name : #FFIArchitecture,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : 'UnifiedFFI'
}

{ #category : #'instance creation' }
FFIArchitecture class >> forCurrentArchitecture [
	^ (self allSubclasses detect: #isActive) uniqueInstance
]

{ #category : #testing }
FFIArchitecture class >> isActive [
	^ false
]

{ #category : #'instance creation' }
FFIArchitecture class >> new [
	self error: 'Use #forCurrentArchitecture'
]

{ #category : #private }
FFIArchitecture class >> uniqueInstance [
	self = FFIArchitecture 
		ifTrue: [ self error: 'I''m an abstract class, use one of my children instead.' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #'default abi' }
FFIArchitecture >> areStructuresSplitIntoRegisters [
	^ false
]

{ #category : #'default abi' }
FFIArchitecture >> computeStructureRegisterPassingLayout: aStructureClass [
	^ nil
	
]

{ #category : #types }
FFIArchitecture >> externalLongType [
	^ ExternalType long
]

{ #category : #types }
FFIArchitecture >> externalULongType [
	^ ExternalType ulong
]

{ #category : #'default abi' }
FFIArchitecture >> floatRegisterCountForParameterPassing [
	^ 0
]

{ #category : #'default abi' }
FFIArchitecture >> floatRegisterSize [
	self subclassResponsibility
]

{ #category : #'default abi' }
FFIArchitecture >> integerRegisterCountForParameterPassing [
	^ 0
]

{ #category : #'default abi' }
FFIArchitecture >> integerRegisterSize [
	self subclassResponsibility
]

{ #category : #types }
FFIArchitecture >> longType [
	^ FFIInt32
]

{ #category : #types }
FFIArchitecture >> longTypeSize [
	^ self externalLongType byteSize
]

{ #category : #'default abi' }
FFIArchitecture >> maxStructureSizeToPassInRegisters [
	^ 0
]

{ #category : #'default abi' }
FFIArchitecture >> returnSingleFloatsAsDoubles [
	self subclassResponsibility
]

{ #category : #'default abi' }
FFIArchitecture >> shadowCallSpaceSize [
	^ 0
]

{ #category : #types }
FFIArchitecture >> ulongType [
	^ FFIUInt32
]
