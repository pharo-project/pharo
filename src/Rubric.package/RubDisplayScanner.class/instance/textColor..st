private
textColor: textColor
	ignoreColorChanges ifTrue: [^ self].
	foregroundColor := textColor