private
resetState
	"Establish the initial conditions for editing the paragraph: place cursor 
	before first character, set the emphasis to that of the first character"

	| b |
	b := self paragraph defaultCharacterBlock.
	textArea markBlock: b pointBlock: b copy.
	self editingState startOfTyping: nil.
	self editingState previousInterval: (1 to: 0).
	self setEmphasisHereFromText