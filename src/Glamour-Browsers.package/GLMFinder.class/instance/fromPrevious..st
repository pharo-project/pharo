scripting
fromPrevious: aPortSymbol
	self flag: 'needs revisiting. it does not work anymore'.
	self scriptTransmission addActiveOrigin: (GLMPort new name: aPortSymbol)