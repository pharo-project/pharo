*Regex-Core
matchesRegexIgnoringCase: regexString
	"Test if the receiver matches a regex.  May raise RxParser>>regexErrorSignal or
	child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^regexString asRegexIgnoringCase matches: self