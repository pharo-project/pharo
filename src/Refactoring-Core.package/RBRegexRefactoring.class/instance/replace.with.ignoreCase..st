searching
replace: aFindString with: aReplaceString ignoreCase: aBoolean
	"Replace all matches of aFindString (regular expression) with aReplaceString, where $0 references the whole match, and $1..$9 the matched groups."
	
	| regex stream |
	regex := RxParser preferredMatcherClass
		for: (RxParser new parse: aFindString)
		ignoreCase: aBoolean.
	stream := aReplaceString readStream.
	matchers add: regex -> stream