tests - begin
testBeginsWithAnyOf
	"We can't test SequenceableCollection directly. However, we can test a sampling of its descendants."
	"self debug: #testBeginsWithAnyOf"
	
	| la oc |
	la := #(1 2 3 4 5 6).
	oc := OrderedCollection new.
	oc add: 1; add: 2; add: 3.
	
	self assert: (la beginsWithAnyOf: #((17) (1) (42))).
	self assert: (la beginsWithAnyOf: #((17) (1 2) (42))).
	self assert: (la beginsWithAnyOf: #((17) (1 2 3) (42))).
	self deny: (la beginsWithAnyOf: #()).
	self deny: (la beginsWithAnyOf: #(())).
	self deny: (la beginsWithAnyOf: #((42))).