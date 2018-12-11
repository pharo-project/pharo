Extension { #name : #BISimpleFormatter }

{ #category : #'*Reflectivity' }
BISimpleFormatter >> visitLiteralVariableNode: aLiteralVariableNode [
	| binding |
	binding := aLiteralVariableNode binding.
	self writeString: binding key storeString, ' -> '.
	aLiteralVariableNode binding value isLiteral
		ifFalse: [ self writeString: '''<an unprintable nonliteral value>''' ]
		ifTrue: [ self writeString:  binding value asString ]
]
