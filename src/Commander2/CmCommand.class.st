"
I am the common superclass to all commands defined in the context of Commander2.

My subclasses need to redefine at least #execute method.
This method defines the behaviour of the command.

Eventually, #canBeRun can be override to check if the command can be run according to its current context.
"
Class {
	#name : #CmCommand,
	#superclass : #CmAbstractCommand,
	#traits : 'CmTWithBasicNameAndDescription + CmTDecorable',
	#classTraits : 'CmTWithBasicNameAndDescription classTrait + CmTDecorable classTrait',
	#instVars : [
		'context'
	],
	#category : #'Commander2-Commands'
}

{ #category : #default }
CmCommand class >> defaultCommandName [ 

	^ 'Unnamed'
]

{ #category : #default }
CmCommand class >> defaultDescription [
	^ 'I''m command and I should provide a nice description for my user.'
]

{ #category : #'instance creation' }
CmCommand class >> forSpec [
	^ self new
		asSpecCommand
]

{ #category : #'instance creation' }
CmCommand class >> forSpecContext: anObject [

	^ self forSpec 
	context: anObject;
	yourself
]

{ #category : #'instance creation' }
CmCommand class >> forSpecWithIconNamed: aSymbol [
	^ self new asSpecCommandWithIconNamed: aSymbol
]

{ #category : #'instance creation' }
CmCommand class >> forSpecWithIconNamed: aSymbol shortcutKey: aKMKeyCombination [
	^ self new
		asSpecCommandWithIconNamed: aSymbol shortcutKey: aKMKeyCombination
]

{ #category : #running }
CmCommand class >> runWith: anObject [
	self new runWith: anObject
]

{ #category : #hooks }
CmCommand >> canBeRun [
	"See my super class comment for this method.
	 By default I return true.
	"
	^ true
]

{ #category : #accessing }
CmCommand >> context [
	"The #context might be a BlockClosure that computes the actual context object when needed.
	 Thus, we send #value which in case of a BlockClosure evaluate it and in case of another
	 Object, simply returns itself.
	"
	^ context value
]

{ #category : #accessing }
CmCommand >> context: anObject [
	context := anObject
]

{ #category : #testing }
CmCommand >> hasContext [
	^ self context isNotNil
]

{ #category : #initialization }
CmCommand >> initialize [
	super initialize.
	self basicName: self class defaultCommandName.
	self basicDescription: self class defaultDescription
]
