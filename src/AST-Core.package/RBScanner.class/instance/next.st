accessing
next
	| token |
	buffer reset.
	tokenStart := stream position.
	token := characterType = #eof 
				ifTrue: 
					[RBToken start: tokenStart + 1	"The EOF token should occur after the end of input"]
				ifFalse: [self scanToken].
	self stripSeparators.
	token comments: self getComments.
	^token