private
lineLengths

	^ self textLines collect: [ :line | self textFont widthOfString: line ].