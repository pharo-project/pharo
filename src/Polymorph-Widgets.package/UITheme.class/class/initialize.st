initialize-release
initialize
	"self initialize"
	self current class = UITheme
		ifTrue: [self current: nil
			"due to change to being abstract"].