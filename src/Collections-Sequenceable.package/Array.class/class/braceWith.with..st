brace support
braceWith: a with: b 
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 2.
	array at: 1 put: a.
	array at: 2 put: b.
	^ array