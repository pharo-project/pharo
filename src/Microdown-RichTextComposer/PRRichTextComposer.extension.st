Extension { #name : #PRRichTextComposer }

{ #category : #'*Microdown-RichTextComposer' }
PRRichTextComposer class >> renderComment: aString of: aClassOrPackage [ 

	| builder |
	builder := MicroDownParser builder.
	aClassOrPackage buildMicroDownUsing: builder withComment: aString.
	self flag: #fixMe.
	"I do not like the asText: the interaction between parser, richtext composer should be 
	more obvious"
	^ [ MicroDownParser asText: builder contents ] on: MicParsingError, MessageNotUnderstood, SubclassResponsibility  do: [ builder contents  ]
]
