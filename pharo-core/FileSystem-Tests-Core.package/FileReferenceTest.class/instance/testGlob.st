tests
testGlob
	| ref children |
	filesystem createDirectory: '/alpha'.
	filesystem createDirectory: '/alpha/beta'.
	filesystem createDirectory: '/alpha/gamma'.
	
	ref := filesystem root.
	children := ref glob: [ :node| true].
	self assert: children size == 4. "including root"
	
	children := ref glob: [ :node| node basename size > 1].
	self assert: children size == 3. "without root"
	
	children := ref glob: [ :node| node basename = #gamma].
	self assert: children size == 1. "gamma"
	self assert: children first basename = #gamma.