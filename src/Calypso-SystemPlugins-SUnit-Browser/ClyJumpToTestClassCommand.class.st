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
	#name : 'ClyJumpToTestClassCommand',
	#superclass : 'SycSingleClassCommand',
	#traits : 'TClyGenerateTestClass',
	#classTraits : 'TClyGenerateTestClass classTrait',
	#instVars : [
		'browser',
		'systemEnvironment'
	],
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'activation' }
ClyJumpToTestClassCommand class >> fullBrowserShortcutActivation [
	<classAnnotation>

	^ ClyShortcutActivation by: $g meta, $j meta for: ClyFullBrowserClassContext
]

{ #category : 'execution' }
ClyJumpToTestClassCommand >> executeRefactoring [

	self systemEnvironment classNamed: (self testClassNameFor: targetClass) ifAbsent: [ (self confirm: 'Do you want to create a test class for: ' , targetClass asString) ifFalse: [ ^ self ] ].

	[ browser selectClass: (self testClassFor: targetClass) ]
		on: ClyInvalidClassForTestClassGeneration
		do: [ :ex | self inform: 'Cannot generate test class for ' , ex baseClass printString , '.' ]
]

{ #category : 'accessing' }
ClyJumpToTestClassCommand >> iconName [
	^#jump
]

{ #category : 'testing' }
ClyJumpToTestClassCommand >> isAvailableInMenuOf: aToolContext [

	^ aToolContext isFullBrowserClassContext
]

{ #category : 'accessing' }
ClyJumpToTestClassCommand >> name [
	^ 'Jump to test class'
]

{ #category : 'accessing' }
ClyJumpToTestClassCommand >> order [

	^ 5
]

{ #category : 'execution' }
ClyJumpToTestClassCommand >> prepareFullExecution [

	super prepareFullExecution.
	browser := self context browser.
	systemEnvironment := self context systemEnvironment
]

{ #category : 'accessing' }
ClyJumpToTestClassCommand >> systemEnvironment [
	^ systemEnvironment
]
