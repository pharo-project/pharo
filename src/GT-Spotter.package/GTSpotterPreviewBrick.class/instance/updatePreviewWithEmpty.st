updating
updatePreviewWithEmpty

	self removeAllBricks.
	self asyncTask
		execute: [ | emptyBrick |
			emptyBrick := GLMLabelBrick new
				text: self defaultNotAvailableText;
				textPosition: #center;
				textColor: Color lightGray;
					with: [ :b | b label
					customFont;
					fontSize: 14 ] ]
		withArguments: {  }
		callback: [ :brick | self compositeBrick: brick ]
		priority: Processor userBackgroundPriority
		delay: 50