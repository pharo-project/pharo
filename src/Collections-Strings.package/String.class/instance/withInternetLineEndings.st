platform conventions
withInternetLineEndings
	"change line endings from CR's and LF's to CRLF's.  This is probably in prepration for sending a string over the Internet"
	
	^self withLineEndings: String crlf