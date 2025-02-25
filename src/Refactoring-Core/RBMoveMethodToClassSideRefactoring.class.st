"
I'm a refactoring to move a method to class side.

My preconditions verify that the method exists and belongs to instance side.

I catch broken references (method senders and direct access to instVar) and fix them.

Example
-----------

Script
```
	(RBMoveMethodToClassSideRefactoring 
		method: (RBTransformationRuleTestData >> #rewriteUsing:) 
		class: RBTransformationRuleTestData) execute.
```
Before refactoring:
```
RBTransformationRuleTestData >> rewriteUsing: searchReplacer 
     rewriteRule := searchReplacer.
     self resetResult.
```
After refactoring:
```
RBTransformationRuleTestData >> rewriteUsing: searchReplacer
     ^ self class rewriteUsing: searchReplace.

RBTransformationRuleTestData class >> rewriteUsing: searchReplacer
    | aRBTransformationRuleTestData |
    aRBTransformationRuleTestData := self new.
    aRBTransformationRuleTestData rewriteRule: searchReplacer.
    aRBTransformationRuleTestData resetResult.
```
"
Class {
	#name : 'RBMoveMethodToClassSideRefactoring',
	#superclass : 'RBMoveMethodToClassRefactoring',
	#instVars : [
		'parseTree'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'refactoring' }
RBMoveMethodToClassSideRefactoring >> accessorsFor: variableName [

	^ ReCreateAccessorsForVariableTransformation
				model: self model
				variable: variableName
				class: class
				classVariable: false
]

{ #category : 'transforming' }
RBMoveMethodToClassSideRefactoring >> addMethod: rbMethod to: aClass toProtocol: protocol [
	aClass addMethod: rbMethod.
	self generateChangesFor:
		(RBAddMethodTransformation
			sourceCode: rbMethod source
			in: aClass
			withProtocol: protocol)
]

{ #category : 'preconditions' }
RBMoveMethodToClassSideRefactoring >> applicabilityPreconditions [

	^ { (RBCondition isMetaclass: class) not }
]

{ #category : 'preconditions' }
RBMoveMethodToClassSideRefactoring >> breakingChangePreconditions [

	^ { (RBCondition definesSelector: method selector in: class classSide) not }
]

{ #category : 'checking' }
RBMoveMethodToClassSideRefactoring >> checkVariableNamed: aString [
	(class whoDefinesInstanceVariable: aString) ifNotNil:
			[^ true].
	(class whoDefinesClassVariable: aString) ifNotNil:
			[^ true].
	^ (self parseTree allDefinedVariables includes: aString)
]

{ #category : 'transforming' }
RBMoveMethodToClassSideRefactoring >> getNewInstSideSource [
	| sender |
	sender := ''.
	method argumentNames isNotEmpty ifTrue: [
		(method selector keywords) with: (method argumentNames )do: [:a :b |
		sender := sender, a, ' ', b, ' ']]
	ifFalse: [ sender := method selector ].
	^ sender, '
	^ self class ', sender
]

{ #category : 'transforming' }
RBMoveMethodToClassSideRefactoring >> getNewSource [

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

{ #category : 'transforming' }
RBMoveMethodToClassSideRefactoring >> getTempName [
	| aString counter tempName |
	counter := 0.
	aString := class canonicalArgumentName.
	tempName := aString.
	[self checkVariableNamed: tempName]
	whileTrue: [ counter := counter + 1.
		tempName := aString , counter asString ].
	^ tempName
]

{ #category : 'accessing' }
RBMoveMethodToClassSideRefactoring >> parseTree [

	parseTree
		ifNil: [ parseTree := class parseTreeForSelector: method selector.
			parseTree ifNil: [ self refactoringError: 'Could not parse method' ]
			].
	^ parseTree doSemanticAnalysis
]

{ #category : 'preconditions' }
RBMoveMethodToClassSideRefactoring >> preconditions [

	^ self applicabilityPreconditions & self breakingChangePreconditions
]

{ #category : 'transforming' }
RBMoveMethodToClassSideRefactoring >> privateTransform [

| oldClass newClass rbMethod rbMethod2 newSource originalProtocol newSource2 |
	newSource := self getNewInstSideSource.
	originalProtocol := method protocolName.
	oldClass := class.
	self removeInstVariableReferences.
	method := class methodFor: method selector.
	newClass := self model classNamed: class name, ' class'.
	newSource2 := self getNewSource.
	rbMethod := self model rbMethodClass for: newClass source: newSource2 selector: method selector.
	rbMethod2 := self model rbMethodClass for: oldClass source: newSource selector: method selector.
	self removeMethod: method selector from: oldClass.
	self addMethod: rbMethod to: newClass toProtocol: originalProtocol.
	self addMethod: rbMethod2 to: oldClass toProtocol: originalProtocol
]

{ #category : 'transforming' }
RBMoveMethodToClassSideRefactoring >> removeInstVariableReferences [

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
			            getter: accessorsRefactoring getterMethodName
			            setter: accessorsRefactoring setterMethodName.
		self convertMethod: method selector for: class using: replacer ]
]

{ #category : 'removing' }
RBMoveMethodToClassSideRefactoring >> removeMethod: selector from: aRBClass [

	self generateChangesFor: 
		(RBRemoveMethodTransformation
			selector: selector
			from: aRBClass)
]

{ #category : 'storing' }
RBMoveMethodToClassSideRefactoring >> storeOn: aStream [
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

{ #category : 'transforming' }
RBMoveMethodToClassSideRefactoring >> temporaryName [
	| aString counter tempName |
	counter := 0.
	aString := class canonicalArgumentName.
	tempName := aString.
	[self checkVariableNamed: tempName]
	whileTrue: [ counter := counter + 1.
		tempName := aString , counter asString ].
	^ tempName
]
