"
I am a refactoring for renaming temporary variables.
This can be applied to method arguments as well.

The variable declaration and all references in this method are renamed.

My precondition verifies that the new name is a valid variable name and not an existing instance or a class variable name
"
Class {
	#name : 'RBRenameArgumentOrTemporaryRefactoring',
	#superclass : 'RBMethodRefactoring',
	#instVars : [
		'selector',
		'interval',
		'oldName',
		'newName',
		'parseTree',
		'definingNode'
	],
	#category : 'Refactoring-Core-Refactorings',
	#package : 'Refactoring-Core',
	#tag : 'Refactorings'
}

{ #category : 'api' }
RBRenameArgumentOrTemporaryRefactoring class >> model: aRBModel rename: oldName to: newName in: aClass selector: aSelector [

	^ self new
		model: aRBModel;
		class: aClass selector: aSelector interval: nil;
		oldName: oldName;
		newName: newName;
		yourself
]

{ #category : 'instance creation' }
RBRenameArgumentOrTemporaryRefactoring class >> model: aRBSmalltalk renameTemporaryFrom: anInterval to: newName in: aClass selector: aSelector [
	^ self new
		model: aRBSmalltalk;
		class: aClass
			selector: aSelector
			interval: anInterval
			newName: newName;
		yourself
]

{ #category : 'api' }
RBRenameArgumentOrTemporaryRefactoring class >> rename: oldName to: newName in: aClass selector: aSelector [

    ^ self new
        class: aClass selector: aSelector interval: nil;
        oldName: oldName;
        newName: newName;
        yourself
]

{ #category : 'instance creation' }
RBRenameArgumentOrTemporaryRefactoring class >> renameTemporaryFrom: anInterval in: aClass selector: aSelector [

	^ self new class: aClass selector: aSelector interval: anInterval
]

{ #category : 'instance creation' }
RBRenameArgumentOrTemporaryRefactoring class >> renameTemporaryFrom: anInterval to: newName in: aClass selector: aSelector [
	^ self new
		class: aClass
		selector: aSelector
		interval: anInterval
		newName: newName
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> applicabilityPreconditions [

	^ self applicabilityPreconditionsIndependentOfTheNewName , self applicabilityPreconditionsOfTheNewName
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> applicabilityPreconditionsIndependentOfTheNewName [
	"Those are the preconditions that should be checked before even asking to the user a new name for the temporary."

	^ { (ReDefinesSelectorsCondition new definesSelectors: (Array with: selector) in: class) }
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> applicabilityPreconditionsOfTheNewName [
	"Those are the preconditions that should be checked after asking to the user a new name for the temporary."

	^ {
		  (ReIsValidInstanceVariableName name: newName).
		  (ReLocalNameConflictCondition
			   class: class
			   variableName: newName
			   definingNode: definingNode
			   parseTree: parseTree) } 
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> breakingChangePreconditions [

	^ { self preconditionCheckVariableName }
]

{ #category : 'initialization' }
RBRenameArgumentOrTemporaryRefactoring >> class: aClass selector: aSelector interval: anInterval [

	class := self classObjectFor: aClass.
	selector := aSelector.
	interval := anInterval
]

{ #category : 'initialization' }
RBRenameArgumentOrTemporaryRefactoring >> class: aClass selector: aSelector interval: anInterval newName: aString [

	self class: aClass selector: aSelector interval: anInterval.
	newName := aString
]

{ #category : 'accessing' }
RBRenameArgumentOrTemporaryRefactoring >> getExistingNameFromInterval [

	| methodSource |
	interval first > interval last ifTrue: [
		self refactoringError: 'You must select a variable name' ].
	methodSource := class sourceCodeFor: selector.
	methodSource size >= interval last ifFalse: [
		self refactoringError: 'Invalid range for variable' ].
	^ methodSource copyFrom: interval first to: interval last.
]

{ #category : 'accessing' }
RBRenameArgumentOrTemporaryRefactoring >> newName: aString [

	newName := aString
]

{ #category : 'accessing' }
RBRenameArgumentOrTemporaryRefactoring >> oldName: aString [
	oldName := aString 
]

{ #category : 'preconditions' }
RBRenameArgumentOrTemporaryRefactoring >> preconditionCheckVariableName [

	^ ReCheckVariableNameShadowingCondition class: class variableName: newName parseTree: parseTree
]

{ #category : 'transforming' }
RBRenameArgumentOrTemporaryRefactoring >> prepareForExecution [

 | variableNode |

 parseTree := class parseTreeForSelector: selector.
 interval 
	ifNil: [
     "Name based rename"
     variableNode := parseTree latestUseOfVariableNamed: oldName
   ] 
	ifNotNil: [
     "Interval based rename"
     oldName := self getExistingNameFromInterval.
     variableNode := [
         self
             whichVariableNode: parseTree
             inInterval: interval
             name: oldName ]
         on: OCCodeError
         do: [
             self refactoringError:
                 'Selected code does not contain any variables or contains multiple of them.' ]
 ].
 (variableNode isNil or: [ variableNode isVariable not ]) ifTrue: [
     self refactoringError: oldName asString , ' isn''t a valid variable' ].
 variableNode name = oldName ifFalse: [
     self refactoringError: 'Invalid selection' ].
 definingNode := variableNode whichUpNodeDefines: oldName.
 definingNode ifNil: [
     self refactoringError:
         oldName asString , ' isn''t defined by the method' ].
]

{ #category : 'transforming' }
RBRenameArgumentOrTemporaryRefactoring >> privateTransform [

	self renameNode: definingNode.
	class compileTree: parseTree
]

{ #category : 'transforming' }
RBRenameArgumentOrTemporaryRefactoring >> renameNode: aParseTree [

	(self parseTreeRewriterClass rename: oldName to: newName) executeTree: aParseTree
]

{ #category : 'accessing' }
RBRenameArgumentOrTemporaryRefactoring >> sourceClass [

	^ class
]
