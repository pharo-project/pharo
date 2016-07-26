dct
idctBlockInt: anArray component: aColorComponent
	^self idctBlockInt: anArray qt: (self qTable at: aColorComponent qTableIndex)