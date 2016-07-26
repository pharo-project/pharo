private
wideCharacterMap
	"used for comparing with WideCharacterSet"
	
	| wide |
	wide := WideCharacterSet new.
	wide addAll: self.
	^wide wideCharacterMap