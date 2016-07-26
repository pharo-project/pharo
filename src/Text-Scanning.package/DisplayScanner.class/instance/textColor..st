text attributes
textColor: textColor
	ignoreColorChanges ifTrue: [^ self].
	foregroundColor := textColor