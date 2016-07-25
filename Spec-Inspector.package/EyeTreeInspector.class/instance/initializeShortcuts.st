initialization
initializeShortcuts
	"initialize the inspector' shortcuts with the dictionary defined in self shortCuts"
	
	self shortCuts keysAndValuesDo: [ :key :value |
		self tree bindKeyCombination: key toAction: value ]