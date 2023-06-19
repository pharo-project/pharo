"
This class is an attempt to see how we can turn extractMethodTestData like method into a static model (e.g., plain simple class definition) 
and a rb model creation. This RB model will be will only in the setup and the setUp should not require to compile code.
"
Class {
	#name : #RBStaticModelCreationTest,
	#superclass : #RBAbstractEnvTestCase,
	#category : #'Refactoring2-Transformations-Tests-Test'
}

{ #category : #tests }
RBStaticModelCreationTest >> testPopulateModel [

	| newModel |
	newModel := self defaultNamespaceClass onEnvironment: RBClassEnvironment new.
	
	newModel defineClass: (ClassDefinitionPrinter oldPharo for: MyClassA) classDefinitionString.
	newModel defineClass: (ClassDefinitionPrinter oldPharo for: MyClassB) classDefinitionString.
	newModel defineClass: (ClassDefinitionPrinter oldPharo for: MyClassC) classDefinitionString.
	newModel name: 'Test'.
	
	
	self assert: (newModel classNamed: #MyClassA) name equals: 'MyClassA'.
	self assert: (newModel classNamed: #MyClassB) superclass name equals: 'MyClassA'.
	self assert: (newModel classNamed: #MyClassC) superclass name equals: 'MyClassA'.

]
