Extension { #name : 'ASTVariableNode' }

{ #category : '*Shout' }
ASTVariableNode >> styleName [

	^ self variable
		  ifNil: [ #default ]
		  ifNotNil: [ :var | var styleNameIn: self ]
]
