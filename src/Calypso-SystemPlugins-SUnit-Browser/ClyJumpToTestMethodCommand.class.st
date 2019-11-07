"
Description
--------------------

I am a command available in the method context menu of Calypso. When activated I'll jump on the test method corresponding to the method. 

If the method does not exists I will create it in the corresponding class.

Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	selectedMethods:			<aCollection>	Methods selected by the user that should have their tests generated.
	testMethodToBrowse:		<aMethod>			The method to browse at the end of the execution of the command.

"
Class {
	#name : #ClyJumpToTestMethodCommand,
	#superclass : #SycMethodCommand,
	#traits : 'TClyGenerateTestClass',
	#classTraits : 'TClyGenerateTestClass classTrait',
	#instVars : [
		'testMethodToBrowse',
		'browser',
		'systemEnvironment'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #testing }
ClyJumpToTestMethodCommand class >> canBeExecutedInContext: aBrowserContext [
	^ aBrowserContext isInstanceSideMethodSelected and: [ aBrowserContext selectedItems anySatisfy: [ :each | (each hasProperty: ClyTestResultProperty) not ] ]
]

{ #category : #activation }
ClyJumpToTestMethodCommand class >> methodMenuActivation [
	<classAnnotation>
	^ CmdContextMenuActivation byItemOf: ClySUnitMethodMenuGroup order: 10 for: ClyMethod asCalypsoItemContext
]

{ #category : #activation }
ClyJumpToTestMethodCommand class >> methodShortcutActivation [
	<classAnnotation>
	^ CmdShortcutActivation by: $h meta , $j meta for: ClyMethod asCalypsoItemContext
]

{ #category : #accessing }
ClyJumpToTestMethodCommand >> defaultMenuItemName [
	^ 'Jump to test method'
]

{ #category : #execution }
ClyJumpToTestMethodCommand >> execute [
	self methods do: [ :method | self generateTestMethodFor: method ].
	
	testMethodToBrowse ifNotNil: [ :testMethod | browser selectMethod: testMethod ]
]

{ #category : #execution }
ClyJumpToTestMethodCommand >> generateTestMethodFor: aMethod [
	[
	| testClass selector |
	testClass := self testClassFor: aMethod methodClass.
	
	selector := self testMethodNameFor: aMethod.
	(testClass includesSelector: selector) ifFalse: [ self generateTestMethodNamed: selector in: testClass ].
	
	testMethodToBrowse := testClass >> selector ] 
		on: ClyInvalidClassForTestClassGeneration
		do: [ :ex | self inform: 'Impossible to create test class for ' , ex baseClass printString , '.' ]
]

{ #category : #action }
ClyJumpToTestMethodCommand >> generateTestMethodNamed: aSymbol in: aClass [
	| body |
	body := '{1}

	self flag: #toImplement.
	self assert: false' format: {aSymbol}.
	aClass compile: body classified: 'test'
]

{ #category : #execution }
ClyJumpToTestMethodCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	browser := aToolContext browser.
	systemEnvironment := aToolContext systemEnvironment
]

{ #category : #accessing }
ClyJumpToTestMethodCommand >> systemEnvironment [
	^ systemEnvironment
]

{ #category : #accessing }
ClyJumpToTestMethodCommand >> testMethodNameFor: aMethod [
	^ aMethod selector asTestSelector
]
