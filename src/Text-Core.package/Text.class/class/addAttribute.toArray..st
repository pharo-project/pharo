private
addAttribute: att toArray: others 
	"Add a new text attribute to an existing set"
	"NOTE: The use of reset and set in this code is a specific
	hack for merging TextKerns."
	att reset.
	^ Array streamContents:
		[:strm | others do:
			[:other | (att dominates: other) ifFalse: [strm nextPut: other]].
		att set ifTrue: [strm nextPut: att]]