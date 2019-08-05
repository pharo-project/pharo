"
Description
--------------------

I am a command available in the class context menu of Calypso. When activated I'll jump on the test class corresponding to the class. 

If the class does not exists I will create it in the corresponding package.
	
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	testClass:		<aClass>		Test class corresponding to the class I was activated with
"
Class {
	#name : #ClyJumpToTestClassCommand,
	#superclass : #SycSingleClassCommand,
	#traits : 'TClyGenerateTestClass',
	#classTraits : 'TClyGenerateTestClass classTrait',
	#instVars : [
		'browser',
		'systemEnvironment'
	],
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #activation }
ClyJumpToTestClassCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation for: ClyClassContextOfFullBrowser
]

{ #category : #activation }
ClyJumpToTestClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>
	
	^ CmdShortcutActivation by: $g meta, $j meta for: ClyClassContextOfFullBrowser
]

{ #category : #accessing }
ClyJumpToTestClassCommand >> defaultMenuIconName [
	^#jump
]

{ #category : #accessing }
ClyJumpToTestClassCommand >> defaultMenuItemName [
	^ 'Jump to test class'
]

{ #category : #execution }
ClyJumpToTestClassCommand >> execute [
	[ browser selectClass: (self testClassFor: targetClass) ]
		on: ClyInvalidClassForTestClassGeneration
		do: [ :ex | self inform: 'Cannot generate test class for ' , ex baseClass printString , '.' ]
]

{ #category : #execution }
ClyJumpToTestClassCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	browser := aToolContext browser.
	systemEnvironment := aToolContext systemEnvironment
]

{ #category : #accessing }
ClyJumpToTestClassCommand >> systemEnvironment [
	^ systemEnvironment
]
