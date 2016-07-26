accessing
memberWithStyleName: aString
	^members detect:[:each | each styleName = aString] ifNone:[]
