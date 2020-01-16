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
	aNode isSequence ifTrue: [ 
		aNode temporaries do: [ :temp |
			self
				checkTemp: temp
				followedBy: aNode statements 
				forCritiquesDo: aBlock ] ]
]

{ #category : #running }
ReTemporaryNeitherReadNorWrittenRule >> checkTemp: aTemp followedBy: statements [
	| isRead isWritten |
	isRead := isWritten := false.
	statements do: [ :statement | 
		statement nodesDo: [ :node | 
			(node isVariable and: [
			 node name = aTemp name ]) ifTrue: [
					isRead    := isRead    or: [ node isWrite not and: [ node isUsed ] ].
					isWritten := isWritten or: [ node isWrite ] ] ] ].
		
	^ (isRead and: isWritten )
]

{ #category : #running }
ReTemporaryNeitherReadNorWrittenRule >> checkTemp: aTemp followedBy: statements forCritiquesDo: aBlock [
	(self checkTemp: aTemp followedBy: statements) ifFalse: [ 
		aBlock cull: (self critiqueFor: aTemp) ]
]

{ #category : #accessing }
ReTemporaryNeitherReadNorWrittenRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReTemporaryNeitherReadNorWrittenRule >> name [
	^ 'Temporary variables not read or not written'
]
