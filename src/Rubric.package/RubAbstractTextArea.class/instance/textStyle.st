accessing text
textStyle
	^textStyle ifNil: [ textStyle := self defaultTextStyle  ]