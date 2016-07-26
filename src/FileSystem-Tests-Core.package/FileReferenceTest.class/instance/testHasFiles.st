tests
testHasFiles
	<publicTest>
	"self debug: #testHasFiles"
	| ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	(filesystem / 'alpha' / 'beta' / 'delta') ensureCreateFile.
	filesystem createDirectory: '/alpha/beta/eta'.
	filesystem createDirectory: '/alpha/gamma'.
	ref := filesystem / 'alpha'.
	
	self deny: ref hasFiles.
	self assert: (ref / 'beta') hasFiles.
	self deny: (ref / 'beta' / 'gamma') hasFiles.