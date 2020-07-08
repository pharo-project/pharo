Extension { #name : #CmAbstractCommandsGroup }

{ #category : #'*NewTools-Debugger' }
CmAbstractCommandsGroup >> commandOrGroupNamed: aString ifNone: aBlock [
	self subclassResponsibility 
]
