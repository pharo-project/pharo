accessing
locateClosureHomeWithContent: aText
	"In case the current context is a BlockContext locate the closureHome and ask the
	user to validate the new context. If closureHome is not found or the user does not 
	validate the new context, return nil. aText is the new content of the current context.
	If the current context is not a BlockContext return it."
	| closureHome |
	
	context isBlockContext ifTrue: [
		closureHome := context activeHome.
		closureHome ifNil: [
			self blockNotFoundDialog: context method with: aText.
		 	^ nil ].
		 (self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withCRs) 
			ifFalse: [ ^ nil ].
		^ closureHome].
	
	^ context