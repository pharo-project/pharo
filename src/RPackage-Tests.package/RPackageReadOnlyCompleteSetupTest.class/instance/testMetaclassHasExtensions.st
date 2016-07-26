test situation
testMetaclassHasExtensions
	"self debug: #testMetaclassHasExtensions"

	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2  name).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #classSideMethodDefinedInP3 ofMetaclassName: a2 name).
	self assert: (p3 includesExtensionSelector: #classSideMethodDefinedInP3 ofClass: a2 class)

	