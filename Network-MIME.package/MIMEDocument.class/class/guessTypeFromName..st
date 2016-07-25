compatibility
guessTypeFromName: url
	"guesses a content type from the url"
	
	| extension |
	extension := url asString copyAfterLast: $..
	^ ZnMimeType
		forFilenameExtension: extension
		ifAbsent: [ nil ]