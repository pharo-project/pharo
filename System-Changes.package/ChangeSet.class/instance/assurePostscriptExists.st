filein/out
assurePostscriptExists
	"Make sure there is a StringHolder holding the postscript.  "

	"NOTE: FileIn recognizes the postscript by the line with Postscript: on it"

	postscript ifNil: [ 
			postscript := '"Postscript:
Leave the line above, and replace the rest of this comment by a useful one.
Executable statements should follow this comment, and should
be separated by periods, with no exclamation points (!).
Be sure to put any further comments in double-quotes, like this one."
' ]