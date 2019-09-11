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
	#category : #'UnifiedFFI-Architecture'
}

{ #category : #'instance creation' }
FFIArchitecture class >> forCurrentArchitecture [
	^ (self allSubclasses detect: #isActive) uniqueInstance
]

{ #category : #testing }
FFIArchitecture class >> isAbstract [ 

	^self name = #FFIArchitecture
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
	self isAbstract  
		ifTrue: [ self error: 'I''m an abstract class, use one of my children instead.' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #types }
FFIArchitecture >> bitTypeAlignment [

	^ 1
]

{ #category : #types }
FFIArchitecture >> bitTypeSize [

	^ self externalBitType byteSize
]

{ #category : #types }
FFIArchitecture >> externalBitType [

	^ ExternalType signedByte
]

{ #category : #types }
FFIArchitecture >> externalLongType [
	^ ExternalType long
]

{ #category : #types }
FFIArchitecture >> externalSizeTType [
	^ ExternalType ulong
]

{ #category : #types }
FFIArchitecture >> externalUBitType [

	^ ExternalType unsignedByte
]

{ #category : #types }
FFIArchitecture >> externalULongType [
	^ ExternalType ulong
]

{ #category : #types }
FFIArchitecture >> longTypeSize [
	^ self externalLongType byteSize
]

{ #category : #'default abi' }
FFIArchitecture >> returnSingleFloatsAsDoubles [
	self subclassResponsibility
]

{ #category : #types }
FFIArchitecture >> sizeTTypeSize [
	^ self externalSizeTType byteSize
]
