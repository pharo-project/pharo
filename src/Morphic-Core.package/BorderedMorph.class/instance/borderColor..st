accessing
borderColor: colorOrSymbolOrNil
	self doesBevels ifFalse:[
		colorOrSymbolOrNil isColor ifFalse:[^self]].
	borderColor = colorOrSymbolOrNil ifFalse: [
		borderColor := colorOrSymbolOrNil.
		self changed].
