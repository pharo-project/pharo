brace support
braceWith: a
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 1.
	array at: 1 put: a.
	^ array