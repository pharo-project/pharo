tests
testChildren
	| ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	
	ref := filesystem / 'alpha'.
	children := ref children.
	self assert: children size = 2.
	children do:
		[:child |
		self assert: child class = FileReference.
		self assert: (child isChildOf: ref).
		self assert: (#('beta' 'gamma') includes: child basename)]