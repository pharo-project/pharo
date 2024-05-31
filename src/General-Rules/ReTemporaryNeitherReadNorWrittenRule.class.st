"
There is something wrong in the method as you have variables that are either nor read or not written (or both).
"
Class {
	#name : 'ReTemporaryNeitherReadNorWrittenRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'accessing' }
ReTemporaryNeitherReadNorWrittenRule class >> group [
	^ 'Optimization'
]

{ #category : 'accessing' }
ReTemporaryNeitherReadNorWrittenRule class >> ruleName [
	^ 'Temporary variables not read or not written'
]

{ #category : 'running' }
ReTemporaryNeitherReadNorWrittenRule >> check: aNode forCritiquesDo: aBlock [
	aNode isTempVariable ifFalse: [ ^ self ].
	aNode isDefinition ifFalse: [ ^ self ].
	aNode variable isReferenced ifFalse: [
		aBlock cull: (self critiqueFor: aNode) ]
]
