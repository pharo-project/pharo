escaping
colorEscape
	self csiEscape.
	blink ifTrue: [ outStream nextPutAll: '5;' ].
	outStream print: currentColor.
	(currentBackground == 0) ifFalse: [
		outStream nextPut: $;; print: currentBackground ].
	outStream nextPut: $m.