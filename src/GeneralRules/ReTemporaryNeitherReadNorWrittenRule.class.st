"
There is something wrong in the method as you have variables that are either nor read or not written (or both).
"
Class {
	#name : #ReTemporaryNeitherReadNorWrittenRule,
	#superclass : #ReNodeBasedRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #running }
ReTemporaryNeitherReadNorWrittenRule >> check: aNode forCritiquesDo: aBlock [
	aNode isVariable ifFalse: [ ^ self ].
	aNode isTemp ifFalse: [ ^ self ].
	aNode isDefinition ifFalse: [ ^ self ].
	aNode variable isReferenced ifFalse: [ 
		aBlock cull: (self critiqueFor: aNode) ]
]

{ #category : #accessing }
ReTemporaryNeitherReadNorWrittenRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReTemporaryNeitherReadNorWrittenRule >> name [
	^ 'Temporary variables not read or not written'
]
