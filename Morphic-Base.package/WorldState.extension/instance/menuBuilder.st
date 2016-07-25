*Morphic-Base
menuBuilder
	^ menuBuilder ifNil: [
		menuBuilder := (PragmaMenuBuilder pragmaKeyword: self discoveredMenuPragmaKeyword model: self)
				menuSpec;
				yourself
	]