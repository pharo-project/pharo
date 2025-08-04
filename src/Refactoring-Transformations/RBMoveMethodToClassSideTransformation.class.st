"
I am responsible for moving a method to class side.

Usage:
```
| transformation |
transformation := (RBMoveMethodToClassSideTransformation
				method: RBMoveMethodParametrizedTest>>#testMoveMethodIntoClass
				class: #RBMoveMethodParametrizedTest)
				generateChanges.
(StRefactoringPreviewPresenter for: transformation) open
```

Preconditions:
- the method exists and belongs to instance side.
"
Class {
	#name : 'RBMoveMethodToClassSideTransformation',
	#superclass : 'RBMoveMethodToClassTransformation',
	#instVars : [
		'parseTree'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'refactoring' }
RBMoveMethodToClassSideTransformation >> accessorsFor: variableName [

	^ RBAddVariableAccessorTransformation
				model: self model
				variable: variableName
				class: class
				classVariable: false
]

{ #category : 'refactoring' }
RBMoveMethodToClassSideTransformation >> addMethod: rbMethod to: aClass toProtocol: protocol [

	self generateChangesFor: (RBAddMethodTransformation
		model: self model
		sourceCode: rbMethod source
		in: aClass
		withProtocol: protocol)
]

{ #category : 'preconditions' }
RBMoveMethodToClassSideTransformation >> applicabilityPreconditions [

	^ {
		  (RBCondition definesSelector: method selector in: class classSide)
			  not.
		  (RBCondition withBlock: [ class isMeta not ]) }
]

{ #category : 'refactoring' }
RBMoveMethodToClassSideTransformation >> checkVariableNamed: aString [
	(class whoDefinesInstanceVariable: aString) ifNotNil:
			[^ true].
	(class whoDefinesClassVariable: aString) ifNotNil:
			[^ true].
	^ (self parseTree allDefinedVariables includes: aString)
]

{ #category : 'refactoring' }
RBMoveMethodToClassSideTransformation >> getNewInstSideSource [
	| sender |
	sender := ''.
	method argumentNames isNotEmpty ifTrue: [
		method selector keywords with: (method argumentNames) do: [:a :b |
		sender := sender, a, ' ', b, ' ']]
	ifFalse: [ sender := method selector ].
	^ sender, '
	^ self class ', sender
]

{ #category : 'refactoring' }
RBMoveMethodToClassSideTransformation >> getNewSource [

	| rewriter node temp |
	temp := self temporaryName.
	node := OCParser parseMethod: method source.
	rewriter := OCParseTreeRewriter new replace: 'self' with: temp.
	(rewriter executeTree: node)
		ifTrue: [
			node := OCParser parseMethod: rewriter tree formattedCode.
			node body addNodeFirst:
				(OCParser parseExpression: temp , ' := self new').
			node body addTemporaryNamed: temp.
			^ node newSource ]
		ifFalse: [ ^ node sourceCode ]
]

{ #category : 'refactoring' }
RBMoveMethodToClassSideTransformation >> parseTree [

	parseTree
		ifNil: [ parseTree := class parseTreeForSelector: method selector.
			parseTree ifNil: [ self refactoringError: 'Could not parse method' ]
			].
	^ parseTree doSemanticAnalysis
]

{ #category : 'transforming' }
RBMoveMethodToClassSideTransformation >> privateTransform [
	| oldClass newClass rbMethod rbMethod2 newSource originalProtocol newSource2 |
	newSource := self getNewInstSideSource.
	originalProtocol := method protocolName.
	oldClass := class.
	self removeInstVariableReferences.
	method := class methodFor: method selector.
	newClass := self model classNamed: class name, ' class'.
	newSource2 := self getNewSource.
	rbMethod := model rbMethodClass for: newClass source: newSource2 selector: method selector.
	rbMethod2 := model rbMethodClass for: oldClass source: newSource selector: method selector.
	self removeMethod: method selector from: oldClass.
	self addMethod: rbMethod to: newClass toProtocol: originalProtocol.
	self addMethod: rbMethod2 to: oldClass toProtocol: originalProtocol
]

{ #category : 'refactoring' }
RBMoveMethodToClassSideTransformation >> removeInstVariableReferences [

	| rbMethod references |
	rbMethod := class methodFor: method selector.
	references := class instanceVariableNames select: [ :e |
		              rbMethod refersToVariable: e ].
	references do: [ :e |
		| replacer accessorsRefactoring |
		accessorsRefactoring := self accessorsFor: e.
		self generateChangesFor: accessorsRefactoring.
		replacer := self parseTreeRewriterClass
			            variable: e
			            getter: accessorsRefactoring getterMethod
			            setter: accessorsRefactoring setterMethod.
		self convertMethod: method selector for: class using: replacer ]
]

{ #category : 'transforming' }
RBMoveMethodToClassSideTransformation >> removeMethod: aSelector from: aRBClass [

	self generateChangesFor: (RBRemoveMethodTransformation
		model: self model
		selector: aSelector
		from: aRBClass )
]

{ #category : 'storing' }
RBMoveMethodToClassSideTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream
		nextPutAll: ' method: '.
	method storeOn: aStream.
	aStream
		nextPutAll: ' class: ';
		nextPutAll: class name.
	aStream
		nextPutAll: ')'
]

{ #category : 'refactoring' }
RBMoveMethodToClassSideTransformation >> temporaryName [
	| aString counter tempName |
	counter := 0.
	aString := class canonicalArgumentName.
	tempName := aString.
	[self checkVariableNamed: tempName]
	whileTrue: [ counter := counter + 1.
		tempName := aString , counter asString ].
	^ tempName
]
