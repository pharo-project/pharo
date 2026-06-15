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
TraitMethodToCompileInfo >> selector [
	^ selector
]

{ #category : 'accessing' }
TraitMethodToCompileInfo >> selector: anObject [
	selector := anObject
]
