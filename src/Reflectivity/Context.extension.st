Extension { #name : #Context }

{ #category : #'*Reflectivity' }
Context >> rftempAt: index put: value [ 
	"same as #tempAt:put:, for recursion stopping metalinks"
	<primitive: 211>
	<metaLinkOptions: #( + optionDisabledLink)>
	"should never reach here, so no rfat:put: needed"
	^self at: index put: value
]
