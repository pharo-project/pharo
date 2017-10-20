tests
testBanLevelCandidatesForPackage
	| package candidates |
	package := self class package.
	candidates := package banLevelCandidates.
	self assert: (candidates includes: package).
	self assert: (candidates includes: ReImageRuleBaner).
	self assert: (candidates includes: RePersistentImageRuleBaner).	