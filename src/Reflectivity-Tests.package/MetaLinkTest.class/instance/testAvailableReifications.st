tests - misc
testAvailableReifications
	self assert: (RBMessageNode new availableReifications includes: #receiver).
	self assert: (RBMessageNode new availableReifications includes: #value).
	self assert: ((ReflectivityExamples classVariableNamed: #ClassVar) availableReifications includes: #name).
	self deny: ((ReflectivityExamples classVariableNamed: #ClassVar) availableReifications includes: #selector).