morphic ui
fillMenu: aMenu fromSpecs: anArray
	anArray do:
		[:pair |
		aMenu add: pair first target: self selector: pair second].
	^ aMenu