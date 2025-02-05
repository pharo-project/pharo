Extension { #name : 'OCVariableNode' }

{ #category : '*Shout' }
OCVariableNode >> styleName [

	^ self variable
		  ifNil: [ #default ]
		  ifNotNil: [ :var | var styleNameIn: self ]
]
