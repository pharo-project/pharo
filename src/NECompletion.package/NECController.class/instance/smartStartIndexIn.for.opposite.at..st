private
smartStartIndexIn: currentText for: smartCharacter  opposite: opposite  at: position
	
	(position - 1) to: 1 by: -1 do: [ :index | | char | 
		char := currentText at: index.
		(char = smartCharacter or: [ char = opposite])
			 ifFalse: [ ^index ] ].
	
	^0