tests
testBlockIsNotClean
	| escpWrite escpRead |
	escpRead := escpWrite := 1.
	self deny: [ self foo ] sourceNode isClean.
	self deny: [ ^ 1 ] sourceNode isClean.
	self deny: [ instVar foo ] sourceNode isClean.
	self deny: [ escpRead foo ] sourceNode isClean.
	self deny: [ escpWrite := 2 ] sourceNode isClean.
	 
	self deny: [[ self foo ]] sourceNode isClean.
	self deny: [[ ^ 1 ]] sourceNode isClean.
	self deny: [[ instVar foo ]] sourceNode isClean.
	self deny: [[ escpRead foo ]] sourceNode isClean.
	self deny: [[ escpWrite := 2 ]] sourceNode isClean.