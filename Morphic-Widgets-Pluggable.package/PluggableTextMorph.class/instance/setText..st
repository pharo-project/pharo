model access
setText: aText
	self okToStyle
		ifFalse: [ self setTextBasic: aText ]
		ifTrue: [
			self setTextBasic: (self styler format: aText).
			aText size < 4096
				ifTrue: [ self styler style: textMorph contents ]
				ifFalse: [ self styler styleInBackgroundProcess: textMorph contents ] ]