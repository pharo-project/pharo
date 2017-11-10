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
