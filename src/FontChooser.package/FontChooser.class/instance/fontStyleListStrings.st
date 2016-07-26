accessing
fontStyleListStrings
	"names of simulated styles are enclosed in parenthesis"

	^ self fontStyleList collect: [:fontFamilyMember | | styleName |
		styleName := fontFamilyMember styleName.
		fontFamilyMember simulated
			ifTrue: ['(', styleName, ')']
			ifFalse: [styleName]].