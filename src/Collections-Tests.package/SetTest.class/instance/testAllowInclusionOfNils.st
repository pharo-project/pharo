tests - with nil
testAllowInclusionOfNils
	| set |
	set := self classToBeTested new.
	set add: nil.
	self assert: (set includes: nil).
	set remove: nil.
	self deny: (set includes: nil)