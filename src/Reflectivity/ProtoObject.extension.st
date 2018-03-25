Extension { #name : #ProtoObject }

{ #category : #'*Reflectivity' }
ProtoObject >> metaLinkOptions [
	^{
	#isNil -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]

{ #category : #'*Reflectivity' }
ProtoObject >> rFwithArgs: argArray executeMethod: compiledMethod [
	"Execute compiledMethod against the receiver and args in argArray"
	"This method is used by reflectivity internally. All meta-links are ignored"
	<disableReflectivity>
	<primitive: 188>
	self primitiveFailed
]

{ #category : #'*Reflectivity' }
ProtoObject >> rfIsEqual: anObject [ 
	"Primitive. Answer whether the receiver and the argument are the same 
	object (have the same object pointer). Do not redefine the message == in 
	any other class! Essential. No Lookup. Do not override in any subclass. 
	See Object documentation whatIsAPrimitive."
	<metaLinkOptions: #( + optionDisabledLink)>
	<primitive: 110>
	self primitiveFailed
]
