tests-perform
testPerformAddRemoveClassInteractively
	| change |
	change := RBRefactoryChangeManager changeFactory addClassDefinition: 'Object subclass: #' , self changeMock name , 'Temporary
	instanceVariableNames: ''''
	classVariableNames: ''''
	poolDictionaries: ''''
	category: ''' , self class category , '''' for: self.
	self perform: change do: [ 
		self assert: (Smalltalk hasClassNamed: change changeClassName).
		self assert: change definedClass name =  change changeClassName.
		self assert: change definedClass isBehavior ].
	self deny: (Smalltalk hasClassNamed: change changeClassName).
	self assert: change definedClass isObsolete
	