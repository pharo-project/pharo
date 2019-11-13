Extension { #name : #RBSimpleFormatter }

{ #category : #'*Reflectivity' }
RBSimpleFormatter >> visitLiteralVariableNode: aLiteralVariableNode [
	| binding |
	binding := aLiteralVariableNode binding.
	self writeString: binding key storeString , ' -> '.
	self
		writeString:
			(aLiteralVariableNode binding value isLiteral
				ifFalse: [ '''<an unprintable nonliteral value>''' ]
				ifTrue: [ binding value asString ])
]
