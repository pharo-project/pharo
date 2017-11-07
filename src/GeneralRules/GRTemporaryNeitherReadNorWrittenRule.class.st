"
There is something wrong in the method as you have variables that are either nor read or not written (or both).
"
Class {
	#name : #GRTemporaryNeitherReadNorWrittenRule,
	#superclass : #ReNodeBasedRule,
	#category : 'GeneralRules'
}

{ #category : #running }
GRTemporaryNeitherReadNorWrittenRule >> check: aNode forCritiquesDo: aBlock [
	aNode isSequence ifTrue: [ 
		aNode temporaries do: [ :temp |
			self
				checkTemp: temp
				followedBy: aNode statements 
				forCritiquesDo: aBlock ] ]
]

{ #category : #running }
GRTemporaryNeitherReadNorWrittenRule >> checkTemp: aTemp followedBy: statements [
	| isRead isWritten |
	isRead := isWritten := false.
	statements do: [ :statement | 
		statement nodesDo: [ :node | 
			(node isVariable and: [
			 node name = aTemp name ]) ifTrue: [
					isRead    := isRead    or: [ node isUsed ].
					isWritten := isWritten or: [ node isWrite ] ] ] ].
		
	^ (isRead and: isWritten )
]

{ #category : #running }
GRTemporaryNeitherReadNorWrittenRule >> checkTemp: aTemp followedBy: statements forCritiquesDo: aBlock [
	(self checkTemp: aTemp followedBy: statements) ifFalse: [ 
		aBlock cull: (self critiqueFor: aTemp) ]
]

{ #category : #accessing }
GRTemporaryNeitherReadNorWrittenRule >> group [
	^ 'Optimization'
]

{ #category : #accessing }
GRTemporaryNeitherReadNorWrittenRule >> name [
	^ 'Temporary variables not read or not written'
]
