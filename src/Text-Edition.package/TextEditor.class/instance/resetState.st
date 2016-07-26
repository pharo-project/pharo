initialize-release
resetState 
	"Establish the initial conditions for editing the paragraph: place caret 
	before first character, set the emphasis to that of the first character,
	and save the paragraph for purposes of canceling."
 
	self editingState markBlock: self paragraph defaultCharacterBlock.
	self editingState pointBlock: self editingState markBlock copy.
	self startOfTyping: nil.
	self editingState previousInterval: (1 to: 0) selection: self nullText.
	self setEmphasisHereFromText.
	selectionShowing := true