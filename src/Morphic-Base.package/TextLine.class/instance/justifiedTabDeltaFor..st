scanning
justifiedTabDeltaFor: spaceIndex 
	"Compute the delta for a tab in a line of justified text, so tab falls 
	somewhere plausible when line is justified."

	| pad extraPad |
	internalSpaces = 0 ifTrue: [^0].
	pad := paddingWidth // internalSpaces.
	extraPad := paddingWidth \\ internalSpaces.
	spaceIndex <= extraPad
		ifTrue: [^spaceIndex * (pad + 1)]
		ifFalse: [^extraPad * (pad + 1) + (spaceIndex - extraPad * pad)]