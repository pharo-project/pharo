*Regex-Core
prefixMatchesRegexIgnoringCase: regexString
	"Test if the receiver's prefix matches a regex.	
	May raise RxParser class>>regexErrorSignal or child signals.
	This is a part of the Regular Expression Matcher package, (c) 1996, 1999 Vassili Bykov.
	Refer to `documentation' protocol of RxParser class for details."

	^regexString asRegexIgnoringCase matchesPrefix: self