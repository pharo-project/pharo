tests
testAllMethodCategoriesIntegratedThrough
	"If this test fails, it may be because method protocols are sorted in #allMethodCategoriesIntegratedThrough. Take care that if the protocols of the class under test are already sorted, you won't see any problem."
	self assert: ((CompiledMethod selectorsInProtocol: 'testing') includes: #isAbstract).
	CompiledMethod allProtocolsUpTo: Object.
	self assert: ((CompiledMethod selectorsInProtocol: 'testing') includes: #isAbstract)