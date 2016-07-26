accessing
clearClipboard
	clipboard = 0 ifTrue: [^self].
	^ self primClearClipboard: clipboard.