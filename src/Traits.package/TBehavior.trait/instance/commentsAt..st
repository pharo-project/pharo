accessing method dictionary
commentsAt:  selector
	"Answer a string representing the first comment in the method associated with selector.  Return an empty string if the relevant source file is not available, or if the method's source code does not contain a comment.  Not smart enough to bypass quotes in string constants, but does map doubled quote into a single quote."


	^self commentsIn:  (self sourceCodeAt: selector) asString.
	
"Behavior commentsAt: #commentsAt:"