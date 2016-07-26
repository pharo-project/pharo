converting
asMinimalRepresentation

	| valid |
	valid := self select: [:e | e isValid ].
	valid size = 0
		ifTrue: [^nil].
	valid size = 1
		ifTrue: [^valid first].
	^valid