testing
isSameRevisionAs: aRGCommentDefinition
	"This method look for equality of the properties of the receiver"
	"A comment validates only its contents and not its stamp"
	
	^(super isSameRevisionAs: aRGCommentDefinition)
		and:[ self content = aRGCommentDefinition content ]