primitives
addClipboardData: data dataFormat: aFormat
	clipboard = 0 ifTrue: 
		[Clipboard clipboardText: data asString.	
		^self].
	self primAddClipboardData: clipboard data: data dataFormat: aFormat