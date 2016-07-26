initialization
lineEndConvention: symbol
	"Set the end of line convention to be used.
	Either #cr, #lf or #crlf (the default)."
	
	self assert: (#(cr lf crlf) includes: symbol).
	lineEnd := String perform: symbol