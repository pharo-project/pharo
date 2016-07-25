stop conditions
cr
	"Answer true. Set up values for the text line interval currently being 
	composed."

	pendingKernX := 0.
	(lastIndex < text size and: [(text at: lastIndex) = Character cr  and: [(text at: lastIndex+1) = Character lf]]) ifTrue: [lastIndex := lastIndex + 1].
	line stop: lastIndex.
	spaceX := destX.
	line paddingWidth: rightMargin - spaceX.
	^true