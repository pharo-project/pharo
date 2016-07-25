gui
showLesson: aLesson withTitle: aString
	self window setLabel: aString.
	self sourceTextModel setText: aLesson lesson.
	self isOpenInWindow ifFalse: [ self open ]
	