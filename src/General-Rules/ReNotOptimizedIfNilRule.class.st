"
ifNil: ifNotNil: ifNil:ifNotNil: ifNotNil:ifNil: is used in a way that it can not be optimized.

This can be fixed by making sure that all arguments are static blocks.

See the method RBMessageNode>>#isInlineIfNil for the exact implementation of the check that the compiler uses


"
Class {
	#name : 'ReNotOptimizedIfNilRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReNotOptimizedIfNilRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReNotOptimizedIfNilRule class >> ruleName [
	^ 'ifNil: ifNotNil: ifNil:ifNotNil: ifNotNil:ifNil: is used in a way that it can not be optimized'
]

{ #category : 'running' }
ReNotOptimizedIfNilRule >> check: aNode forCritiquesDo: aBlock [
	aNode isMessage ifFalse: [  ^ self ].
	(#(ifNil: ifNotNil: ifNil:ifNotNil: ifNotNil:ifNil:) includes: aNode selector) ifFalse: [^ self].
	aNode isInlineIfNil ifFalse: [
		aBlock cull: (self critiqueFor: aNode) ]
]
