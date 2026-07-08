"
I am an object storing informations needed to copy a method from the trait composition to one of its user. 

## History

When a trait is updated, we have to rebuild the method dictionary of its users. In the past, we installed the methods of the trait composition by iterating over the selectors of the trait composition and we asked the trait composition for the information needed to install the method.

This was slow for traits with a lot of methods and users because it traversed the trait composition multiple times for each selector.

Now, the trait composition can produce a collection of my instance by traversing only once the trait composition and by adding all the needed information to my instance.

## Implementation

I can store the different informations needed to install a method from a trait to a user. I'll store:
- The trait defining the method
- The method from the trait that is installed
- The source code to install (this is not the source code of the method because we can update the code with aliases for example)
- The selector of the method
- I also know if I represent an explicit requirement or subclass responsibility and I also know if I represent a trait conflict for a selector
"
Class {
	#name : 'TraitMethodCompilationInfo',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'definingTrait',
		'compiledMethod',
		'sourceCode',
		'protocol',
		'isConflicting'
	],
	#category : 'Traits-Class-Builder',
	#package : 'Traits',
	#tag : 'Class-Builder'
}

{ #category : 'comparing' }
TraitMethodCompilationInfo >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ selector = anObject selector and: [
		  definingTrait = anObject definingTrait and: [ sourceCode = anObject sourceCode and: [ self protocol = anObject protocol ] ] ]
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> compiledMethod [
	^ compiledMethod
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> compiledMethod: anObject [
	compiledMethod := anObject
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> definingTrait [
	^ definingTrait
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> definingTrait: anObject [
	definingTrait := anObject
]

{ #category : 'comparing' }
TraitMethodCompilationInfo >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ selector hash bitXor: (definingTrait hash bitXor: (sourceCode hash bitXor: protocol hash))
]

{ #category : 'initialization' }
TraitMethodCompilationInfo >> initialize [

	super initialize.
	isConflicting := false
]

{ #category : 'testing' }
TraitMethodCompilationInfo >> isConflicting [

	^ isConflicting
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> isConflicting: anObject [
	isConflicting := anObject
]

{ #category : 'testing' }
TraitMethodCompilationInfo >> isExplicitRequirement [

	^ self compiledMethod isRequired
]

{ #category : 'printing' }
TraitMethodCompilationInfo >> printOn: aStream [
	"Generate a string representation of the receiver based on its instance variables."

	super printOn: aStream.

	aStream
		nextPutAll: ' [ ';
		print: definingTrait;
		nextPutAll: '>>';
		print: selector.

	self compiledMethod ifNotNil: [ :cm | cm isRequired ifTrue: [ aStream nextPutAll: ' - Explicit requirement' ] ].

	aStream nextPutAll: ']'
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> protocol [

	^ protocol ifNil: [ self compiledMethod protocolName ]
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> protocol: anObject [
	protocol := anObject
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> selector [
	^ selector
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> selector: anObject [
	selector := anObject
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> sourceCode [

	^ sourceCode
]

{ #category : 'accessing' }
TraitMethodCompilationInfo >> sourceCode: anObject [
	sourceCode := anObject
]
