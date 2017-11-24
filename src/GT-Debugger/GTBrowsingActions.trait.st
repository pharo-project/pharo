"
I provide various actions for browsing a Context object. These actions are loaded by the debugger in the context menu of a stack.
"
Trait {
	#name : #GTBrowsingActions,
	#category : #'GT-Debugger-Actions'
}

{ #category : #actions }
GTBrowsingActions >> browseClassRefs [

	self currentContext ifNotNil: [ :aContext || class|
		class := aContext methodClass theNonMetaClass.
		class isTrait 
			ifTrue: [ self systemNavigation browseAllUsersOfTrait: class ]
			ifFalse: [ self systemNavigation browseAllCallsOnClass: class ] ]

]

{ #category : #actions }
GTBrowsingActions >> browseClassVarRefs [
	| cls |
	cls := self currentClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation  browseClassVarRefs: cls]
]

{ #category : #actions }
GTBrowsingActions >> browseClassVariables [
	"Browse the class variables of the selected class."
	| cls |
	cls := self currentClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation  browseClassVariables: cls]

]

{ #category : #actions }
GTBrowsingActions >> browseInstVarRefs [
	| cls |
	cls := self currentClassOrMetaClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation browseInstVarRefs: cls]
]

{ #category : #actions }
GTBrowsingActions >> browseMessages [
	"Present a menu of all messages sent by the current message. 
	Open a message set browser of all implementors of the message chosen."

	self currentMessageName ifNotNil: [   
		self systemNavigation browseAllImplementorsOf: self currentMessageName ]
]

{ #category : #actions }
GTBrowsingActions >> browseMethodFull [
	"Create and schedule a full Browser and then select the current class and message."

	self currentClassOrMetaClass ifNotNil: [ 
		Smalltalk tools browser 
			openOnClass: self currentClassOrMetaClass
			selector: self currentMessageName ]
]

{ #category : #actions }
GTBrowsingActions >> browseSendersOfMessages [
	"Present a menu of the current message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self currentMessageName ifNotNil: [   
		self systemNavigation browseAllSendersOf: self currentMessageName ]
]

{ #category : #actions }
GTBrowsingActions >> browseVersions [
	"Create and schedule a message set browser on all versions of the 
	currently selected message selector."

	Smalltalk tools versionBrowser browseVersionsForClass: self currentClassOrMetaClass selector: self currentMessageName
]

{ #category : #accessing }
GTBrowsingActions >> currentClass [

	^ self currentContext methodClass
]

{ #category : #accessing }
GTBrowsingActions >> currentClassOrMetaClass [

	^ self currentContext methodClass
]

{ #category : #accessing }
GTBrowsingActions >> currentContext [
	
	^ self debugger currentContext
]

{ #category : #accessing }
GTBrowsingActions >> currentMessageName [

	^ self currentContext methodSelector
]

{ #category : #actions }
GTBrowsingActions >> fileOutMessage [
	"Put a description of the selected message on a file"

	self currentMessageName ifNotNil: [
		Cursor write showWhile:
			[ self currentClassOrMetaClass fileOutMethod: self currentMessageName ] ]
]

{ #category : #actions }
GTBrowsingActions >> inspectInstances [
	"Inspect all instances of the selected class."

	self currentClassOrMetaClass ifNotNil: [ :aClass |
		aClass theNonMetaClass inspectAllInstances ]
]

{ #category : #actions }
GTBrowsingActions >> inspectSubInstances [
	"Inspect all instances of the selected class and all its subclasses"

	self currentClassOrMetaClass ifNotNil: [ :aClass |
		aClass theNonMetaClass inspectSubInstances ]
]

{ #category : #actions }
GTBrowsingActions >> methodHierarchy [
	"Create and schedule a method browser on the hierarchy of implementors."

	self systemNavigation 
			methodHierarchyBrowserForClass: self currentClassOrMetaClass 
			selector: self currentMessageName

]
