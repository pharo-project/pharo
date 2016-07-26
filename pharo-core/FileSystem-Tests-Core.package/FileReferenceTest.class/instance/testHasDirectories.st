tests
testHasDirectories
	<publicTest>
	"self debug: #testHasDirectories"
	| ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	
	(filesystem / 'alpha' / 'beta' / 'delta') ensureCreateFile.
	
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	
	self assert: ref hasDirectories.
	self deny: (ref / 'beta') hasDirectories.
	self deny: (ref / 'beta' / 'gamma') hasDirectories.