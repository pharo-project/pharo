Extension { #name : #RBVariableNode }

{ #category : #'*Shout' }
RBVariableNode >> styleName [

	self variable ifNil: [ ^ #default ].
	^ self variable styleNameIn: self
]
