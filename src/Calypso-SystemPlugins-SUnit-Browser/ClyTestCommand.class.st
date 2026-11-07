"
I am a root of hierarchy of commands to run any kind of tests
"
Class {
	#name : 'ClyTestCommand',
	#superclass : 'ClyCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClyTestCommand class >> isAbstract [
	^self = ClyTestCommand
]

{ #category : 'execution' }
ClyTestCommand >> applyCommandResult [
	super applyCommandResult.
	"Following code forces browser update to not wait for deferred updating logic
	to give immediate feedback with actual test icons.
	Without this code the icons updating is visually delayed"

	self context activeQueryView dataSource runUpdate
]

{ #category : 'context menu support' }
ClyTestCommand >> iconName [

	^self testResult iconName
]

{ #category : 'execution' }
ClyTestCommand >> notifyUserAboutResults: testResult with: message [

	GrowlMorph showTestResult: testResult with: message
]

{ #category : 'tests' }
ClyTestCommand >> testResult [
	self subclassResponsibility
]
