brace support
braceWith: a with: b with: c with: d
	"This method is used in compilation of brace constructs.
	It MUST NOT be deleted or altered."

	| array |
	array := self new: 4.
	array at: 1 put: a.
	array at: 2 put: b.
	array at: 3 put: c.
	array at: 4 put: d.
	^ array