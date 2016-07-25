accesing
named: aColorName
	
	^ ColorRegistry  at: aColorName asSymbol ifAbsent: nil 