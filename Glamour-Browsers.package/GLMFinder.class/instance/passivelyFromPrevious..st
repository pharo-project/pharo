scripting
passivelyFromPrevious: aPortSymbol
	self flag: 'needs revisiting. it does not work anymore'.
	self scriptTransmission addPassiveOrigin: (GLMPort new name: aPortSymbol)