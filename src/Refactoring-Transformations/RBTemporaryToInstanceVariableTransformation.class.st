"
I am a responsible for changing a temporary variable to an instance variable.

My preconditions verify that this variable is not yet used as an instance variable in this class.

The temporary variable is added to the class definition and removed from the temporary declaration in this method .

If this instance variable is already used in a subclass it will be removed from that class, because subclasses already inherit this attribute.

The temporary variables with the same name in hierarchy will be removed, and replaced with the new instance variable.

Example
--------------------

Example: 
| transformation |
transformation := (RBTemporaryToInstanceVariableRefactoring 
				class: MyClassA
    			selector: #someMethod
    			variable: 'log') 
				transform.
(ChangesBrowser changes: transformation model changes changes) open

Script refactoring:
```
(RBTemporaryToInstanceVariableRefactoring 
    class: MyClassA
    selector: #someMethod
    variable: 'log') execute
```
Before refactoring:
```
Object subclass: #MyClassA
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'example'

MyClassA >> someMethod 
    |log aNumber|
    log := self newLog.
    log isNil.
    aNumber := 5.

MyClassA >> anotherMethod
    #(4 5 6 7) do: [:e | | log |
        log := e ]

MyClassA subclass: #MyClassB
	instanceVariableNames: 'log'
	classVariableNames: ''
	package: 'example'
```
After refactoring:
```
Object subclass: #MyClassA
	instanceVariableNames: 'log'
	classVariableNames: ''
	package: 'example'

MyClassA >> someMethod 
    | aNumber |
    log := self newLog.
    log isNil.
    aNumber := 5.

MyClassA >> anotherMethod
    #(4 5 6 7) do: [:e | 
        log := e ]

MyClassA subclass: #MyClassB
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'example'
```
"
Class {
	#name : 'RBTemporaryToInstanceVariableTransformation',
	#superclass : 'RBMethodTransformation',
	#instVars : [
		'temporaryVariableName'
	],
	#category : 'Refactoring-Transformations-Model-Unused',
	#package : 'Refactoring-Transformations',
	#tag : 'Model-Unused'
}

{ #category : 'instance creation' }
RBTemporaryToInstanceVariableTransformation class >> class: aClass selector: aSelector variable: aVariableName [
	^ self new
		class: aClass
		selector: aSelector
		variable: aVariableName
]

{ #category : 'instance creation' }
RBTemporaryToInstanceVariableTransformation class >> model: aRBSmalltalk class: aClass selector: aSelector variable: aVariableName [
	^ self new
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			variable: aVariableName;
		yourself
]

{ #category : 'preconditions' }
RBTemporaryToInstanceVariableTransformation >> applicabilityPreconditions [

	^ {
		  (RBCondition definesSelector: selector in: class).
		  (RBCondition
			   definesInstanceVariable: temporaryVariableName asString
			   in: class) not.
		  (RBCondition withBlock: [
			   self checkForValidTemporaryVariable.
			   true ]) }
]

{ #category : 'preconditions' }
RBTemporaryToInstanceVariableTransformation >> checkForValidTemporaryVariable [
	| parseTree |
	parseTree := class parseTreeForSelector: selector.
	parseTree ifNil: [ self refactoringError: 'Could not create parse tree for ' class name , '>>' , selector ].
	(parseTree allTemporaryVariables includes: temporaryVariableName)
		ifFalse:
			[self refactoringError: temporaryVariableName
						, ' isn''t a valid temporary variable name'].
	(parseTree allArgumentVariables includes: temporaryVariableName)
		ifTrue:
			[self refactoringError: temporaryVariableName , ' is a block parameter'].
	(OCReadBeforeWrittenTester isVariable: temporaryVariableName
		readBeforeWrittenIn: parseTree)
			ifTrue:
				[self
					refactoringWarning: ('<1s> is read before it is written.<n>Proceed anyway?'
							expandMacrosWith: temporaryVariableName)]
]

{ #category : 'instance creation' }
RBTemporaryToInstanceVariableTransformation >> class: aClass selector: aSelector variable: aVariableName [
	class := self model classObjectFor: aClass.
	selector := aSelector.
	temporaryVariableName := aVariableName
]

{ #category : 'executing' }
RBTemporaryToInstanceVariableTransformation >> privateTransform [
	self removeTemporaryOfClass: class.
	class allSubclasses do: [ :cls |
		(cls definesInstanceVariable: temporaryVariableName)
			ifTrue: [ cls removeInstanceVariable: temporaryVariableName ]
			ifFalse: [ self removeTemporaryOfClass: cls ] ].
	class addInstanceVariable: temporaryVariableName
]

{ #category : 'removing' }
RBTemporaryToInstanceVariableTransformation >> removeTemporaryOfClass: aClass [
	aClass selectors do: [ :aSymbol | self removeTemporaryOfMethod: aSymbol in: aClass ]
]

{ #category : 'removing' }
RBTemporaryToInstanceVariableTransformation >> removeTemporaryOfMethod: aSelector in: aClass [
	| parseTree matcher aMethod |
	aMethod := aClass methodFor: aSelector.
	parseTree := aMethod parseTree.
	parseTree ifNil: [ self refactoringError: 'Could not parse method' ].
	( matcher := self parseTreeRewriterClass removeTemporaryNamed: temporaryVariableName )
		executeTree: parseTree.
	aMethod compileTree: matcher tree
]

{ #category : 'storing' }
RBTemporaryToInstanceVariableTransformation >> storeOn: aStream [
	aStream nextPut: $(.
	self class storeOn: aStream.
	aStream nextPutAll: ' class: '.
	class storeOn: aStream.
	aStream
		nextPutAll: ' selector: #';
		nextPutAll: selector;
		nextPutAll: ' variable: ''';
		nextPutAll: temporaryVariableName;
		nextPut: $'.
	aStream nextPut: $)
]
