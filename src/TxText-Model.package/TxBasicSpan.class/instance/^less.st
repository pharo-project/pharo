testing
< aSpan
	"Answer whether the receiver is closer to the start of the text than than argument.
	NB: both spans must belong to the same text"
	
	| forward backward |
	
	text == aSpan text ifFalse: [ self error: 'Cannot compare positions belonging to different text' ].

	"equivalent case first"
	self == aSpan ifTrue: [  ^ false ].
	
	"go both ways: forward and backward.. and see where aSpan is relative to receiver"
	forward := backward := self.
	 [
		forward := forward next.
		backward := backward prev.
	
		forward == aSpan ifTrue: [ ^ true ].
		backward == aSpan ifTrue: [ ^ false ].
		
		"reached start"
		backward isAtStart ifTrue: [ ^ true ]. 
		"reached end"
		forward isAtEnd ifTrue: [ ^ false ].
		
	] repeat.
	