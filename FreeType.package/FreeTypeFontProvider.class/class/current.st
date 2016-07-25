accessing
current
	"
	current := nil.
	TimeProfileBrowser onBlock: [FreeTypeFontProvider current]
	"
	^current 
		ifNil:[
			current := self new.
			current updateFromSystem]