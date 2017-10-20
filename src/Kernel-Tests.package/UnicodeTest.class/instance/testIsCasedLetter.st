tests-categorization
testIsCasedLetter
	"The Cased letter category, LC, is empty"
	self aRandomSelectionOfCharactersDo: [ :ch | 
		self deny: (Unicode isCasedLetter: ch) ]