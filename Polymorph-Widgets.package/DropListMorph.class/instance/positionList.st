private
positionList
	"Position the list morph to fit on the display."

	|height topSpace bottomSpace pos|
	height := self listHeight.
	topSpace := self boundsInWorld top - self world top.
	bottomSpace := self world bottom - self boundsInWorld bottom.
	pos := height <= bottomSpace
		ifTrue: [#below]
		ifFalse: [height <= topSpace
			ifTrue: [#above]
			ifFalse: [bottomSpace >= topSpace
				ifTrue: [height := bottomSpace. #below]
				ifFalse: [height := topSpace. #above]]].
	pos = #above
		ifTrue: [self buttonMorph roundedCorners: (self roundedCorners copyWithoutAll: #(1 4)).
				self roundedCorners: (self roundedCorners copyWithoutAll: #(1 4)).
				self listMorph
					bounds: (self boundsInWorld topLeft - (0 @ height) extent: self width @ height)]
		ifFalse: [self buttonMorph roundedCorners: (self roundedCorners copyWithoutAll: #(1 2 3)).
				self roundedCorners: (self roundedCorners copyWithoutAll: #(2 3)).
				self listMorph
					bounds: (self boundsInWorld bottomLeft extent: self width @ height)]