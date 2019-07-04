Class {
	#name : #RBRenameTemporaryVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #testing }
RBRenameTemporaryVariableTransformationTest >> testExistingVariable [

	| transformation |
	transformation := (RBAddMethodTransformation
							sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
							in: self changeMock name
							withProtocol: #accessing)
							transform.
	
	self shouldFail: (RBRenameTemporaryVariableTransformation 
							model: transformation model
							rename: #temp to: #bar
							in: self changeMock name
							selector: #foo)
]

{ #category : #testing }
RBRenameTemporaryVariableTransformationTest >> testRefactoring [

	| transformation class |
	transformation := (RBAddMethodTransformation
							sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
							in: self changeMock name
							withProtocol: #accessing)
							transform.
	
	transformation := (RBRenameTemporaryVariableTransformation 
							model: transformation model
							rename: #temp to: #temp2
							in: self changeMock name
							selector: #foo)
							asRefactoring transform.
	
	self assert: transformation model changes changes size equals: 2.			
	
	class := transformation model classNamed: self changeMock name.
	self assert: (class directlyDefinesMethod: #foo).		
	self assert: (class parseTreeFor: #foo) temporaries size equals: 2.
	self assert: ((class parseTreeFor: #foo) temporaries anySatisfy: [ :e | e name = #temp2 ])
]

{ #category : #testing }
RBRenameTemporaryVariableTransformationTest >> testRename [

	| transformation |
	transformation := (RBRenameTemporaryVariableTransformation 
							rename: #rules to: #asdf
							in: #RBLintRuleTestData
							selector: #openEditor)
							transform.
							
	self assert: ((transformation model classNamed: #RBLintRuleTestData) parseTreeFor: #openEditor)
		  equals: (self parseMethod: 'openEditor
								| asdf |
								asdf := self failedRules.
								asdf isEmpty ifTrue: [^self].
								asdf size == 1 ifTrue: [^asdf first viewResults]')
]

{ #category : #testing }
RBRenameTemporaryVariableTransformationTest >> testTransform [

	| transformation class |
	transformation := (RBAddMethodTransformation
							sourceCode: 'foo
									| temp bar |
									bar := 5.
									temp := bar * bar.
									Transcript show: temp printString; cr.
									^temp * temp'
							in: self changeMock name
							withProtocol: #accessing)
							transform.
	
	transformation := (RBRenameTemporaryVariableTransformation 
							model: transformation model
							rename: #temp to: #temp2
							in: self changeMock name
							selector: #foo)
							transform.
	
	self assert: transformation model changes changes size equals: 2.			
	
	class := transformation model classNamed: self changeMock name.
	self assert: (class directlyDefinesMethod: #foo).		
	self assert: (class parseTreeFor: #foo) temporaries size equals: 2.
	self assert: ((class parseTreeFor: #foo) temporaries anySatisfy: [ :e | e name = #temp2 ])
]

{ #category : #testing }
RBRenameTemporaryVariableTransformationTest >> testVariableDoesNotExist [

	self shouldFail: (RBRenameTemporaryVariableTransformation 
							rename: #rule
							to: #name
							in: #RBLintRuleTestData
							selector: #openEditor)
]
