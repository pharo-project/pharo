parsing
parseDirectoryListing: aString
	| stream files line tokens |
	stream := aString readStream.
	files := OrderedCollection new.
	[stream atEnd] whileFalse:
		[line := stream nextLine.
		tokens := line findTokens: ' '.
		tokens size > 2 ifTrue: [files add: tokens last]].
	^ files