tests
testAddClassInteractively
	| change |
	change := RBRefactoryChangeManager changeFactory addClassDefinition: 'TestCase subclass: #' , self class name , '
	instanceVariableNames: ''instVar''
	classVariableNames: ''ClassVar''
	poolDictionaries: ''PoolDict''
	category: ''' , self class category , '''' for: self.
	self assert: change controller = self.
	self assert: change superclassName = self class superclass name.
	self assert: change changeClassName = self class name.
	self assert: change changeClass = self class.
	self assert: change isMeta not.
	self assert: change instanceVariableNames size = 1.
	self assert: change instanceVariableNames first = 'instVar'.
	self assert: change classVariableNames size = 1.
	self assert: change classVariableNames first = 'ClassVar'.
	self assert: change sharedPoolNames size = 1.
	self assert: change sharedPoolNames first = 'PoolDict'.
	self assert: change category = self class category.
	self universalTestFor: change