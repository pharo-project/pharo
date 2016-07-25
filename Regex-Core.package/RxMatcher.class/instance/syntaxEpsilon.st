double dispatch
syntaxEpsilon
	"Double dispatch from the syntax tree. Match empty string. This is unlikely
	to happen in sane expressions, so we'll live without special epsilon-nodes."

	^RxmSubstring new
		substring: String new
		ignoreCase: ignoreCase