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
