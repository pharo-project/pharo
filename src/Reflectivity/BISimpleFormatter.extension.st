Extension { #name : #BISimpleFormatter }

{ #category : #'*Reflectivity' }
BISimpleFormatter >> visitLiteralVariableNode: aLiteralVariableNode [
	| binding |
	binding := aLiteralVariableNode binding.
	self writeString: binding key storeString , ' -> '.
	self
		writeString:
			(aLiteralVariableNode binding value isLiteral
				ifFalse: [ '''<an unprintable nonliteral value>''' ]
				ifTrue: [ binding value asString ])
]
