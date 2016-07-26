accessing
insetColor
	owner ifNil:[^self color].
	^ self colorForInsets