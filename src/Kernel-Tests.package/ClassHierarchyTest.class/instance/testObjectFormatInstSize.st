tests
testObjectFormatInstSize
	"check that #instSize is in sync with #allInstVarNames"
	self assert: (SystemNavigation default allBehaviors 
		allSatisfy: [ :cls | cls instSize = cls allInstVarNames size ])