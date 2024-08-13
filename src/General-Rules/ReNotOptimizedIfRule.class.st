"
ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue: is used in a way that it can not be optimized.

This can be fixed by making sure that all arguments are static blocks.

See the method RBMessageNode>>#isInlineIf for the exact implementation of the check that the compiler uses
"
Class {
	#name : 'ReNotOptimizedIfRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReNotOptimizedIfRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReNotOptimizedIfRule class >> ruleName [
	^ 'ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue: is used in a way that it can not be optimized'
]

{ #category : 'running' }
ReNotOptimizedIfRule >> check: aNode forCritiquesDo: aBlock [
	aNode isMessage ifFalse: [  ^ self ].
	(#(ifTrue: ifFalse: ifTrue:ifFalse: ifFalse:ifTrue:) includes: aNode selector) ifFalse: [^ self].
	aNode isInlineIf ifFalse: [
		aBlock cull: (self critiqueFor: aNode) ]
]
