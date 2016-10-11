testing
testOriginWithRequiredMethodInTraitChain
	"check that the origin of a explicit requirement is the trait defining this method, even if we check
this from another *trait* using the first trait. Tr2 uses Tr1, Tr1 defines a explicit requirement #foo, the
origin of Tr2>>foo is Tr1"

	| tr1 tr2 |
	tr1 := self createTraitNamed: #TTT1 uses: {}.
	tr1 compile: 'foo ^ self explicitRequirement'.
	self assert: (tr1 >> #foo) origin == tr1.
	tr2 := self createTraitNamed: #TTT2 uses: {tr1}.
	self assert: (tr2 >> #foo) origin == tr1