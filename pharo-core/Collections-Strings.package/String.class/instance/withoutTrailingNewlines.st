converting
withoutTrailingNewlines
	"Return a copy of the receiver with any combination of cr/lf characters at the end removed"

	^ self trimRight: [ :char | char = Character cr or: [ char = Character lf ] ]