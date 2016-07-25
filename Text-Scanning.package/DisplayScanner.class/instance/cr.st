stop conditions
cr
	"When a carriage return is encountered, simply increment the pointer 
	into the paragraph."

	pendingKernX := 0.
	lastDisplayableIndex := lastIndex - 1.
	(lastIndex < text size and: [(text at: lastIndex) = CR and: [(text at: lastIndex+1) = Character lf]])
		ifTrue: [lastIndex := lastIndex + 2]
		ifFalse: [lastIndex := lastIndex + 1].
	^false