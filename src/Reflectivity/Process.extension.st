Extension { #name : #Process }

{ #category : #'*Reflectivity' }
Process >> rfeffectiveProcess [
	"same as #effectiveProcess but for internal use for metalink activation"
	<metaLinkOptions: #( + optionDisabledLink)>
	^effectiveProcess ifNil: [self]
]
