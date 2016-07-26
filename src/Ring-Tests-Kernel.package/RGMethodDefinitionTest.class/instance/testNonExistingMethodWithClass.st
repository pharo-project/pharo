testing
testNonExistingMethodWithClass
	"method does not exist in OrderedCollection"
	| newMethod newClass |
	
	newClass:= RGClassDefinition named: #OrderedCollection.
	newClass withMetaclass.
	
	newMethod:= (RGMethodDefinition named: #foo) parent: newClass theMetaClass.
	newMethod sourceCode: 'foo
									^true'.
									
	self assert: (newMethod isMethod).
	self assert: (newMethod selector == #foo).
	self assert: (newMethod isMetaSide).
	self assert: (newMethod protocol = nil).
	
	self assert: (newMethod parent = newClass theMetaClass).
	self assert: (newMethod parentName = newClass theMetaClass name).
	
	self assert: (newMethod environment == Smalltalk globals).
	self assert: (newMethod realClass = OrderedCollection class).
	self assert: (newMethod compiledMethod isNil).