copying
copyWithFirst: newElement 
	"Answer a copy of the receiver that is 1 bigger than the receiver with newElement as the first element."

	| newIC |
	newIC := self species ofSize: self size + 1.
	newIC 
		replaceFrom: 2
		to: self size + 1
		with: self
		startingAt: 1.
	newIC at: 1 put: newElement.
	^ newIC