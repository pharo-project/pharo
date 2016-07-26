dct
primIdctBlockInt: anArray component: aColorComponent
	^self primIdctInt: anArray qt: (self qTable at: aColorComponent qTableIndex)