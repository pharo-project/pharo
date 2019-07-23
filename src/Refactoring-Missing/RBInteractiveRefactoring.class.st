"
I am a interactive layer to  use refactoring without use explicity from commands.

Instance Variables
-----------------------------------------------------------------
method: CompiledMethod
interval: Interval
node:	RBNode
	
Example
-----------------------------------------------------------------
I can be used in playground, for example:

GIVEN:
======
Object subclass: #Example
	instanceVariableNames: ''
	classVariableNames: ''
	package: 'Refactoring-InteractiveLayer'
	
Example >> extracted
	|a|
	a := 5 + 5.
   a := 'sdaf' , a.
	^ a
	
THEN EXECUTE: 
=============

|interactive|

interactive := RBInteractiveRefactoring method: #extracted class: Example interval: (17 to: 26).
interactive executeExtractMethod 

RESULT: 
=======

Yoou can access to  preview of extract method

"
Class {
	#name : #RBInteractiveRefactoring,
	#superclass : #Object,
	#instVars : [
		'refactoring',
		'method',
		'sourceNode'
	],
	#category : #'Refactoring-Missing'
}

{ #category : #'instance creation' }
RBInteractiveRefactoring class >> method: method [
	^ self new 
		method: method .
]

{ #category : #'instance creation' }
RBInteractiveRefactoring class >> method: aMethod class: aClass interval: anInterval [
	"Initialize method and node of this class given:
	aMethod: a string of method name
	aClass: class that owns the given method"
	
	|instance|
	instance := self new.
	instance method:  (instance compiledMethodOf: aMethod from: aClass).
	instance sourceNode: (instance method astForStylingInCalypso bestNodeFor: anInterval).
	instance sourceNode ifNil: [ instance sourceNode: instance method astForStylingInCalypso ].
	^ instance
]

{ #category : #'instance creation' }
RBInteractiveRefactoring class >> sourceNode: source method: method [
	^ self new 
		sourceNode: source; 
		method: method .
]

{ #category : #accessing }
RBInteractiveRefactoring >> compiledMethodOf: aMethod from: aClass [
	"Return compiled method given a method name (sylbol) and a class"
	
	^ aClass >> aMethod
]

{ #category : #execution }
RBInteractiveRefactoring >> executeExtractMethod [
	self
		executeThis: [ self extractMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBInteractiveRefactoring >> executeFindAndReplace [
	"self
		executeThis: [ 
			self findAndReplaceMethod.
			self executeRefactoringWith: #primitiveExecute ]"
]

{ #category : #execution }
RBInteractiveRefactoring >> executeInlineMethod [
	self
		executeThis: [self inlineMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBInteractiveRefactoring >> executeInlineTemp [
	self
		executeThis: [ self inlineTemp.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBInteractiveRefactoring >> executePushDown [
	self
		executeThis: [ 
			self pushDownMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBInteractiveRefactoring >> executePushUp [
	self
		executeThis: [ 
			self pushUpMethod.
			self executeRefactoringWith: #primitiveExecute ]
]

{ #category : #execution }
RBInteractiveRefactoring >> executeRefactoringWith: aSymbol [
	"Execute aSymbol refactoring method"
	
	[refactoring perform: aSymbol.
	^ refactoring ] on:RBRefactoringError do: [ :e | e signal]
]

{ #category : #execution }
RBInteractiveRefactoring >> executeThis: aBlock [
	[
		self executionOfRequirements.
		aBlock value.
		self showChanges 
	] 
	on: RBRefactoringError 
	do: [ :e | UIManager default alert: e messageText ]
]

{ #category : #execution }
RBInteractiveRefactoring >> executionOfRequirements [
	"If the refactoring needs some requirement like newName or other thing you must
	 do this here"
]

{ #category : #refactoring }
RBInteractiveRefactoring >> extractMethod [
	"Return extract method refactoring"
	
	| selectedInterval dialog|
	selectedInterval := sourceNode sourceInterval.
	refactoring := RBExtractMethodRefactoring	
		extract: selectedInterval from: method selector in: method origin.
	self setUpOptionToUseExistingMethodDuring: refactoring.
	self setUpOptionToOverrideExistingMethodDuring: refactoring.
	refactoring setOption: #methodName toUse: [ :ref :methodName |
		dialog := SycMethodNameEditor openOn: methodName withRefactoring: ref.
		dialog cancelled ifTrue: [  CmdCommandAborted signal ].
		methodName]
]

{ #category : #refactoring }
RBInteractiveRefactoring >> extractTempWithName: newTempName [
	"Return extract temporary refactoring using method, node and newTempName"
	^ RBExtractToTemporaryRefactoring	
		extract: sourceNode sourceInterval
		to: newTempName
		from: method selector
		in: method origin
]

{ #category : #refactoring }
RBInteractiveRefactoring >> findAndReplaceMethod [
	"|dialog|
	refactoring :=  RBFindAndReplaceMethodRefactoring 
		find: method .
	refactoring setOption: #listToSearch toUse: [ :met :ref |
		dialog := RBFindAndReplacePreview method: met refactoring: ref.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ].
		ref].
	^ refactoring "
]

{ #category : #test }
RBInteractiveRefactoring >> inlineMethod [
	"Return inline method refactoring using method and node"
	
	refactoring := RBInlineMethodRefactoring	
		inline: sourceNode sourceInterval
		inMethod: method selector
		forClass: method origin.
		
	^ refactoring
]

{ #category : #refactoring }
RBInteractiveRefactoring >> inlineTemp [
	"Return inline temporary refactoring using method and node"
	
	| assignment |
	assignment := sourceNode methodNode assignmentNodes
		detect: [ :each | each variable = sourceNode ].
	refactoring := RBInlineTemporaryRefactoring
		inline: assignment sourceInterval
		from: method selector
		in: method origin.
		
	^ refactoring
]

{ #category : #accessing }
RBInteractiveRefactoring >> method [
	^ method
]

{ #category : #accessing }
RBInteractiveRefactoring >> method: anObject [
	method := anObject
]

{ #category : #refactoring }
RBInteractiveRefactoring >> pushDownMethod [
	"Return push down method refactoring using method"
	|dialog|
	refactoring :=  RBPushDownMethodRefactoring 
		pushDown: method  
		from: method first origin.
	refactoring setOption: #selectorsFromView toUse: [ :ref |
		dialog := RBPushDownPreview on: ref.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ]].
	
	^ refactoring 
]

{ #category : #refactoring }
RBInteractiveRefactoring >> pushUpMethod [
	"Return push up method refactoring using method"
	|dialog|
	refactoring :=  RBPullUpMethodRefactoring 
		pullUp: method  
		from: method first origin.
	refactoring setOption: #superClass toUse: [ :ref |
		dialog := RBPullUpPreview on: ref.
		dialog cancelled ifTrue: [ CmdCommandAborted signal ]].
	
	^ refactoring 
]

{ #category : #accessing }
RBInteractiveRefactoring >> refactoring [
	^ refactoring
]

{ #category : #accessing }
RBInteractiveRefactoring >> refactoring: anObject [
	refactoring := anObject
]

{ #category : #refactoring }
RBInteractiveRefactoring >> renameTempWithName: newTempName [
	"Return rename temporary refactoring using method, node and newTempName"
	
	 ^ RBRenameTemporaryRefactoring	
		renameTemporaryFrom: sourceNode sourceInterval
		to: newTempName
		in: method origin
		selector: method selector
]

{ #category : #initialization }
RBInteractiveRefactoring >> setUpOptionToOverrideExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #alreadyDefined toUse:  [ :ref :aClass :selector | 
		ref refactoringWarning: 'Method ', selector printString, ' will override method in ', aClass name]. 
]

{ #category : #initialization }
RBInteractiveRefactoring >> setUpOptionToUseExistingMethodDuring: aRefactoring [

	aRefactoring setOption: #useExistingMethod toUse:  [ :ref :selector | 
		UIManager default 
			confirm: 'Do you want use existing method ', selector printString, '?'
			label: 'Warninig']. 	
	
]

{ #category : #initialization }
RBInteractiveRefactoring >> setupMethodNameFor: aRefactoring toReturn: aSelector [ 
	| options |
	options := aRefactoring options copy.
	options at: #methodName
		put: 
			[:ref :aMethodName | 
			aMethodName
				selector: aSelector;
				yourself].
	aRefactoring options: options
]

{ #category : #preview }
RBInteractiveRefactoring >> showChanges [
	(ChangesBrowserPresenter changes: {refactoring}) open
]

{ #category : #accessing }
RBInteractiveRefactoring >> sourceNode [
	^ sourceNode
]

{ #category : #accessing }
RBInteractiveRefactoring >> sourceNode: anObject [
	sourceNode := anObject
]
