tests
testHasChildren
	<publicTest>
	"self debug: #testHasChildren"
	| ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/beta/delta'.
	filesystem createDirectory: '/alpha/gamma'.
	
	ref := filesystem / 'alpha'.
	
	self assert: ref hasChildren.
	self assert: (ref / 'beta') hasChildren.
	self deny: (ref / 'beta' / 'delta') hasChildren.
	self deny: (ref / 'beta' / 'gamma') hasChildren.