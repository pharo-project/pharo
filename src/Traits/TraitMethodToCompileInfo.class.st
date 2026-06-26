Class {
	#name : 'TraitMethodToCompileInfo',
	#superclass : 'Object',
	#instVars : [
		'selector',
		'definingTrait',
		'compiledMethod',
		'sourceCode',
		'protocol'
	],
	#category : 'Traits-Compositions',
	#package : 'Traits',
	#tag : 'Compositions'
}

{ #category : 'comparing' }
TraitMethodToCompileInfo >> = anObject [
	"Answer whether the receiver and anObject represent the same object."

	self == anObject ifTrue: [ ^ true ].
	self class = anObject class ifFalse: [ ^ false ].
	^ selector = anObject selector and: [
			  definingTrait = anObject definingTrait and: [
				  compiledMethod = anObject compiledMethod and: [ sourceCode = anObject sourceCode and: [ self protocol = anObject protocol ] ] ] ]
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> compiledMethod [
	^ compiledMethod
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> compiledMethod: anObject [
	compiledMethod := anObject
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> definingTrait [
	^ definingTrait
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> definingTrait: anObject [
	definingTrait := anObject
]

{ #category : 'comparing' }
TraitMethodToCompileInfo >> hash [
	"Answer an integer value that is related to the identity of the receiver."

	^ selector hash bitXor: (definingTrait hash bitXor: (compiledMethod hash bitXor: (sourceCode hash bitXor: protocol hash)))
]

{ #category : 'testing' }
TraitMethodToCompileInfo >> isExplicitRequirement [

	^ self compiledMethod isRequired
]

{ #category : 'printing' }
TraitMethodToCompileInfo >> printOn: aStream [
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
TraitMethodToCompileInfo >> protocol [

	^ protocol ifNil: [ self compiledMethod protocolName ]
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> protocol: anObject [
	protocol := anObject
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> selector [
	^ selector
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> selector: anObject [
	selector := anObject
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> sourceCode [

	^ sourceCode
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> sourceCode: anObject [
	sourceCode := anObject
]
