editing keys
hiddenInfo
	"In TextLinks, TextDoits, TextColor, and TextURLs, there is hidden info.  Return the entire string that was used by Cmd-6 to create this text attribute.  Usually enclosed in < >."

	| attrList |
	attrList := self text attributesAt: (self pointIndex + self markIndex)//2.
	attrList do: [:attr |
		attr forTextActionInfoDo: [ :info |
			^ self selection asString, '<', info, '>']].
	"If none of the above"
	attrList do: [:attr |
		attr forTextColorDo: [ :color |
			^ self selection asString, '<', color printString, '>']].
	^ self selection asString, '[No hidden info]'