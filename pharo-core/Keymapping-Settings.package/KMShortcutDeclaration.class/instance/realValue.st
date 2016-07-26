accessing
realValue
	"Answer the value of realValue"

	^ declaration
		ifNil: [
			declaration := KMShortcutSetting for: self shortcutName in: self categoryName]