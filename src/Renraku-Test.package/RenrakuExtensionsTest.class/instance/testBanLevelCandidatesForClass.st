tests
testBanLevelCandidatesForClass
	| class candidates |
	class := self class.
	candidates := class banLevelCandidates.
	self assert: (candidates includes: class).
	self assert: (candidates includes: class package).
	self assert: (candidates includes: ReImageRuleBaner).
	self assert: (candidates includes: RePersistentImageRuleBaner).	