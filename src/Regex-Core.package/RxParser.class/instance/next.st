private
next
	"Advance the input storing the just read character
	as the lookahead."

	input atEnd
		ifTrue: [lookahead := #epsilon]
		ifFalse: [lookahead := input next]