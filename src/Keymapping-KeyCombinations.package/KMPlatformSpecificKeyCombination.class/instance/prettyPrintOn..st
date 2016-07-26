printing
prettyPrintOn: aStream

	self platform = #MacOSX
		ifTrue: [ shortcut prettyPrintOn: aStream ]
		ifFalse: [ shortcut printOn: aStream ]