private
createClipboard
	clipboard = 0 ifTrue: [^self].
	^ self primCreateClipboard.