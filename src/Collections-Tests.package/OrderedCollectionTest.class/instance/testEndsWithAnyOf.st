tests - begin
testEndsWithAnyOf
	"We can't test SequenceableCollection directly. However, we can test a sampling of its descendants."
	"self debug: #testEndsWithAnyOf"
	| la oc |
	la := #(6 5 4 3 2 1).
	oc := OrderedCollection new.
	oc add: 3; add: 2; add: 1.
	
	self assert: (la endsWithAnyOf: #((17) (1) (42))).
	self assert: (la endsWithAnyOf: #((17) (2 1) (42))).
	self assert: (la endsWithAnyOf: #((17) (3 2 1) (42))).
	self deny: (la endsWithAnyOf: #()).
	self deny: (la endsWithAnyOf: #(())).
	self deny: (la endsWithAnyOf: #((42))).