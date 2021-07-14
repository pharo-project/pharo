Class {
	#name : #RBRenameTemporaryVariableTransformationTest,
	#superclass : #RBTransformationTest,
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #tests }
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
							withProtocols: {#accessing})
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
