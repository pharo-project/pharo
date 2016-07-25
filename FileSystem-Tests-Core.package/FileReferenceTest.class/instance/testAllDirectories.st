tests
testAllDirectories
	"allDirectories returns all folders recursively nested in a reference"
	<publicTest>
	"self debug: #testAllDirectories"
	| ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	
	ref := filesystem / 'alpha'.
	children := ref allDirectories.
	"all children returns the directories: '/alpha', '/alpha/beta', and '/alpha/gamma'."
	self assert: children size = 4.
	
	children do:
		[:child |
		self assert: child class = FileReference.
		self assert: (ref = child or: [ref contains: child])].
	self assert: (children collect: [:ea | ea basename]) = #('alpha' 'beta' 'gamma' 'delta')