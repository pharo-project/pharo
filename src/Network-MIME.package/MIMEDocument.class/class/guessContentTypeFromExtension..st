compatibility
guessContentTypeFromExtension: ext
	"guesses a content type from the extension"
	^(self guessTypeFromExtension: ext) asString