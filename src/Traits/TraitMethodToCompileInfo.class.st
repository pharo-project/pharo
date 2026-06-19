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
