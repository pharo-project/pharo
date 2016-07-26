testing
testExistingMethodWithoutClass
	"its parent class is not assigned only its name"
	| newMethod |
	
	newMethod:= (RGMethodDefinition named: #add:)
					parentName: #OrderedCollection;
					selector: #add:; 
					isMetaSide: false;
					protocol: 'adding'; 
					sourceCode: 'add: newObject
									^self addLast: newObject'.
	
	self assert: (newMethod isMethod).
	self assert: (newMethod selector == #add:).
	self assert: (newMethod isMetaSide not).
	self assert: (newMethod protocol = #adding).
	self assert: (newMethod fullName = 'OrderedCollection>>add:').
	self assert: (newMethod sourceCode = 'add: newObject
									^self addLast: newObject').
	self assert: (newMethod hasStamp not).
	self assert: (newMethod parent isNil).
	self assert: (newMethod parentName == #OrderedCollection).
	
	self assert: (newMethod environment == Smalltalk globals).
	self assert: (newMethod realClass = OrderedCollection).
	self assert: (newMethod compiledMethod = (OrderedCollection compiledMethodAt: #add:))