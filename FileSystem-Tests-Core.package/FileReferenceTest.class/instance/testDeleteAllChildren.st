tests
testDeleteAllChildren
	"allChildren returns all the files and folders recursively nested in a reference"
	<publicTest>
	"self debug: #testDeleteAllChildren"
	| ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	
	ref := filesystem / 'alpha'.
	ref deleteAllChildren.
	
	self assert: ref exists.
	self deny: (ref / 'beta') exists.
	self deny: (ref / 'beta' / 'delta') exists.
	self deny: (ref / 'beta' / 'gamma') exists.