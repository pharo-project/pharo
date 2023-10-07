Extension { #name : 'RBVariableNode' }

{ #category : '*Shout' }
RBVariableNode >> styleName [

	^ self variable
		  ifNil: [ #default ]
		  ifNotNil: [ :var | var styleNameIn: self ]
]
