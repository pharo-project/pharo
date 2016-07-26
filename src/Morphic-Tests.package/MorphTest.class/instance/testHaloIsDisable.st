as yet unclassified
testHaloIsDisable
	| isHaloEnable |
	isHaloEnable := Morph halosEnabled.
	Morph halosEnabled: false.
	self assert: (Morph new addHalo: nil) ~= (Smalltalk globals at: #HaloMorph).
	Morph halosEnabled: isHaloEnable