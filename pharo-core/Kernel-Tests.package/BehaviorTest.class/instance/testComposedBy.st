tests
testComposedBy
	"tests the #isComposedBy: aTrait method"
	

	self deny: (Object isComposedBy: TSortable).
	self assert: (SequenceableCollection isComposedBy: TSortable).
	self assert: (Trait3 isComposedBy: Trait2).
	self deny: (Trait2 isComposedBy: Trait3).
	self deny: (Trait3 isComposedBy: Object).
	