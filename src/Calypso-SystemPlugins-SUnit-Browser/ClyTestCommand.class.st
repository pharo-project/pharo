"
I am a root of hierarchy of commands to run any kind of tests
"
Class {
	#name : #ClyTestCommand,
	#superclass : #CmdCommand,
	#category : #'Calypso-SystemPlugins-SUnit-Browser-Commands'
}

{ #category : #testing }
ClyTestCommand class >> isAbstract [
	^self = ClyTestCommand 
]

{ #category : #execution }
ClyTestCommand >> applyResultInContext: aToolContext [
	super applyResultInContext: aToolContext.
	"Following code forces browser update to not wait for deferred updating logic
	to give immediate feedback with actual test icons.
	Without this code the icons updating is visually delayed"
	
	aToolContext activeQueryView dataSource runUpdate.
]

{ #category : #'context menu support' }
ClyTestCommand >> defaultMenuIcon [
	
	^self testResult createIcon
]

{ #category : #execution }
ClyTestCommand >> notifyUserAboutResults: testResult with: message [

	| color |	
	color := Color gray.
	testResult hasPassed
		ifTrue: [ color := Color green ].
	testResult hasFailures
		ifTrue: [ color := Color yellow ].
	testResult hasErrors
		ifTrue: [ color := Color red ].
		
	GrowlMorph 
		openWithLabel: message
		contents: testResult printString
		backgroundColor: color
		labelColor: Color black
]

{ #category : #tests }
ClyTestCommand >> testResult [
	self subclassResponsibility 
]
