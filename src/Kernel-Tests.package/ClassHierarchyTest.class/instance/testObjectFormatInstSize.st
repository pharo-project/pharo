tests
testObjectFormatInstSize
	| classes |
	"check that #instSize is in sync with #allInstVarNames"
	self assert:
			(SystemNavigation default allBehaviors
				allSatisfy: [ :cls | cls instSize = cls allInstVarNames size ]).

	"instSpec is only 0 for classes without any inst vars or variable fields"
	classes := SystemNavigation default allBehaviors
		select: [ :cls | cls isTrait not and: [ cls instSpec = 0 ] ].
	self assert: (classes allSatisfy: [ :each | each instVarNames isEmpty or: [ each isVariable not ] ])