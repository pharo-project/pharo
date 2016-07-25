source code management
author
	"Answer the author of the current version of the receiver. retrieved from the sources or changes file. Answer the empty string if no time stamp is available."

	"(CompiledMethod compiledMethodAt: #author) author"

	self timeStamp ifNotEmpty: [:s | 
		|subS|
		subS := s substrings first. 
		subS first isLetter ifTrue:[^subS]].
	^''