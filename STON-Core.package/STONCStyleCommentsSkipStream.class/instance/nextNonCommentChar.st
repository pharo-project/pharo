private
nextNonCommentChar
	| char |
	char := stream next.
	(self insideString and: [ char = $\ ])
		ifTrue: [ 
			escape := true. 
			^ char ].
	(char = $') | (char = $")
		ifTrue: [ 
			^ self handleStringDelimiter: char ].
	escape := false. 
	^ (char = $/ and: [ self insideString not ])
		ifTrue: [ 
			self consumeComment.
			stream next ]
		ifFalse: [ char ]
