private
findRealFont
	"for now just get a strike"
	"^((TextStyle named: StrikeFont defaultFontKey) fontOfPointSize: pointSize)
		emphasized: emphasis"
	^LogicalFontManager current bestFontFor: self