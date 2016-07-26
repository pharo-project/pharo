tests
testAllChildren
	"allChildren returns all the files and folders recursively nested in a reference"
	<publicTest>
	"self debug: #testAllChildren"
	| ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	
	ref := filesystem / 'alpha'.
	children := ref allChildren.
	"all children returns the nodes: '/alpha', '/alpha/beta',  '/alpha/beta/delta', and '/alpha/gamma'."
	self assert: children size = 4.
	children do:
		[:child |
		self assert: child class = FileReference.
		self assert: (ref = child or: [ref contains: child])].
	self assert: (children collect: [:ea | ea basename]) = #('alpha' 'beta' 'gamma' 'delta')