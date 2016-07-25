testing
testWithPoolDictionaries
	| newClass poolVar |
	
	newClass:= (RGClassDefinition named:  #Text)
						addSharedPoolNamed: #TextConstants;
						yourself.

	self assert: (newClass sharedPools size = 1).
	self assert: (newClass sharedPoolNames size = 1).
	self assert: (newClass allSharedPools size = 1).  "no hierarchy"
	self assert: (newClass allSharedPoolNames size = 1).
	
	poolVar:= newClass sharedPoolNamed: #TextConstants.
	self assert: (poolVar notNil).
	self assert: (poolVar isPoolVariable).
	self assert: (poolVar isVariable).
	self assert: (poolVar parent = newClass).
	self assert: (poolVar parentName == newClass name).
	self assert: (poolVar realClass = Text).
	
	newClass withMetaclass.
	self assert: (newClass theMetaClass allSharedPoolNames size = 1).
	
	newClass removeSharedPoolNamed: #TextConstants.
	self assert: (newClass sharedPools isEmpty).