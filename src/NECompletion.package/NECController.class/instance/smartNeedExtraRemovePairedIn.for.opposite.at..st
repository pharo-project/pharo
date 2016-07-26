private
smartNeedExtraRemovePairedIn: currentText for: smartCharacter opposite: opposite at: position
	"Test if we need to remove an extra character when removed a paired smart character.
	 A paired smart character is any smart character who has an opposite who is diferent to itself: [], ()"
	
	| startIndex countSmart countOpposite |
	
	countSmart := 0.
	countOpposite := 0.
	startIndex := self
		smartStartIndexIn: currentText
		for: smartCharacter
		opposite: opposite
		at: position.
		
	(currentText allButFirst: startIndex) 
		do: [ :char | 
			char = smartCharacter
				ifTrue: [ countSmart := countSmart + 1 ].
			char = opposite
				ifTrue: [ countOpposite := countOpposite + 1 ] ].
	
	(countSmart > countOpposite and: [ (countOpposite - countSmart) odd ])
		ifTrue: [ ^ false ].
		
	^true