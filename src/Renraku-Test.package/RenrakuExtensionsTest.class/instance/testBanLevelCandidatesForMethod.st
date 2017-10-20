tests
testBanLevelCandidatesForMethod
	| method candidates |
	method := thisContext  method.
	candidates := method banLevelCandidates.
	self assert: (candidates includes: method).
	self assert: (candidates includes: self class).
	self assert: (candidates includes: self class package).
	self assert: (candidates includes: ReImageRuleBaner).
	self assert: (candidates includes: RePersistentImageRuleBaner).	