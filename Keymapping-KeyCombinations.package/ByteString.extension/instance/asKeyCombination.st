*Keymapping-KeyCombinations
asKeyCombination
	self size > 1 
		ifTrue: [ self error: 'Shortcuts only take a single letter'].
	^ KMSingleKeyCombination from: self first