"
A DebugAction is the entry point for creating debugging actions.

A new debugging action is created by subclassing it and implementing, by default, the method executeAction (This can be configured by using the method actionSelector).  An id must also be provided uniquely identifying the action among all the others.

The initialize method should only set default values or initialize attributes that are independent of the debugger or session. The others must be initilized in the method forDebugger:.

A debugging action has the following lifecycle: 
	- the action is created using #new and #initialized is called
	- #appliesToDebugger:, and for contextual actions #appliesToContext: are called
	- if the actions applies to the current situation #forDebugger: is used to set the debugger
	- #execute is called when the user triggers the action.
	- when the debugger updates its action this process is repeated.
"
Class {
	#name : #DebugAction,
	#superclass : #Object,
	#instVars : [
		'label',
		'order',
		'icon',
		'keymap',
		'category',
		'debugger',
		'needsValidation',
		'needsUpdate',
		'needsSeparatorAfter',
		'actionSelector'
	],
	#category : #'Debugger-Actions-Actions'
}

{ #category : #accessing }
DebugAction class >> asDebugAction [

	^ self new
]

{ #category : #accessing }
DebugAction class >> defaultIcon [

	^ nil
]

{ #category : #'instance creation' }
DebugAction class >> forDebugger: aDebugger [

	^ self new forDebugger: aDebugger
]

{ #category : #actions }
DebugAction >> actionSelector [

	^ actionSelector ifNil: [ self defaultActionSelector ]
]

{ #category : #testing }
DebugAction >> appliesToContext: aContext [

	^ true
]

{ #category : #testing }
DebugAction >> appliesToCurrentContext [

	^ self appliesToContext: self currentContext 
]

{ #category : #testing }
DebugAction >> appliesToDebugger: aDebugger [

	^ true
]

{ #category : #accessing }
DebugAction >> asDebugAction [

	^ self
]

{ #category : #accessing }
DebugAction >> category [
	"Return the kind of debugger action. It has nothing to do with class category or the old method category now method protocol."
	^ category ifNil: [ self defaultCategory ]
]

{ #category : #accessing }
DebugAction >> category: anObject [
	"Set the kind of debugger action. It has nothing to do with class category or the old method category now method protocol."
	category := anObject
]

{ #category : #'accessing-context' }
DebugAction >> currentContext [

	^ self debugger currentContext
]

{ #category : #accessing }
DebugAction >> debugger [

	^ debugger
]

{ #category : #accessing }
DebugAction >> debugger: anObject [

	debugger := anObject
]

{ #category : #actions }
DebugAction >> defaultActionSelector [

	^ #executeAction
]

{ #category : #accessing }
DebugAction >> defaultCategory [
	"Return the default kind of debugger action. It has nothing to do with class category or the old method category now method protocol."
	^ nil
]

{ #category : #accessing }
DebugAction >> defaultIcon [

	^ self class defaultIcon
]

{ #category : #accessing }
DebugAction >> defaultKeyText [
	"I return a string based on which a default shortcut is created for this action.
	
	I am still present to ensure backwards compatibility.
	The method #defaultKeymap should be overridden instead."
	self flag: 'Enable the deprecation once all usages of #defaultKeyText are removed.'.
	"self
		deprecated: 'Override #defaultKeymap instead to set a keymap object as shortcut.'
		on: '2016-09-05'
		in: #Pharo6."
	
	^ nil
	
]

{ #category : #accessing }
DebugAction >> defaultKeymap [

	^ nil
]

{ #category : #accessing }
DebugAction >> defaultLabel [

	^ ''
]

{ #category : #accessing }
DebugAction >> defaultOrder [

	^ 0
]

{ #category : #accessing }
DebugAction >> enabled [

	^ true
]

{ #category : #actions }
DebugAction >> execute [

	self precondition ifTrue: [ 
		self preAction.
		self perform: self actionSelector.
		self postAction ] 
]

{ #category : #actions }
DebugAction >> executeAction [
	"By default do nothing."
	
]

{ #category : #initialization }
DebugAction >> forDebugger: aDebugger [

	self debugger: aDebugger
]

{ #category : #accessing }
DebugAction >> help [
	^ self subclassResponsibility
]

{ #category : #accessing }
DebugAction >> icon [

	^ icon ifNil: [ self defaultIcon ]
]

{ #category : #accessing }
DebugAction >> icon: anObject [

	icon := anObject
]

{ #category : #accessing }
DebugAction >> id [

	^ nil
]

{ #category : #'accessing-context' }
DebugAction >> interruptedContext [
	
	^ self debugger interruptedContext
	
]

{ #category : #accessing }
DebugAction >> keyText [
	"I return the character associated with this shortcut.
	I return an uppercase character if the keymap has shift as a modifier."

	^  self keymap 
		ifNil: [ 
			"Backwards compatibility."
			self defaultKeyText ]
		ifNotNil: [ |currentCharacter|
			"Needed as current menus display 'Shift' only if the charater is in uppercase."
			currentCharacter := self keymap currentCharacter.
			(self keymap printString includesSubstring: 'Shift')
				ifTrue: [  currentCharacter asString asUppercase ]
				ifFalse: [ currentCharacter asString ] ]
]

{ #category : #accessing }
DebugAction >> keymap [

	^ keymap ifNil: [ 
		self defaultKeymap ifNil: [ 
			"Code to ensure backwards compatibility with the previous
			solution that used #defaultKeyText."
			self defaultKeyText ifNotNil: [ :aString |
				aString first isUppercase 
					ifTrue: [ aString first meta shift ]
					ifFalse: [ aString first meta ] ] ] ]
]

{ #category : #accessing }
DebugAction >> keymap: aKeymap [

	keymap := aKeymap
]

{ #category : #accessing }
DebugAction >> label [

	^ label ifNil: [ self defaultLabel ]
]

{ #category : #accessing }
DebugAction >> label: aString [

	label := aString
]

{ #category : #accessing }
DebugAction >> needsSeparatorAfter [

	^ needsSeparatorAfter ifNil: [ false ]
]

{ #category : #accessing }
DebugAction >> needsSeparatorAfter: aBoolean [

	needsSeparatorAfter := aBoolean
]

{ #category : #accessing }
DebugAction >> needsUpdate [

	^ needsUpdate ifNil: [ true ]
]

{ #category : #accessing }
DebugAction >> needsUpdate: aBoolean [

	needsUpdate := aBoolean
]

{ #category : #accessing }
DebugAction >> needsValidation [

	^ needsValidation ifNil: [ true ]
]

{ #category : #accessing }
DebugAction >> needsValidation: anObject [

	needsValidation := anObject
]

{ #category : #actions }
DebugAction >> notifyDebuggingActionAboutToExecute [
	
	self debugger announce: (DebugActionAboutToExecute forDebuggingAction: self)
]

{ #category : #actions }
DebugAction >> notifyDebuggingActionExecuted [
	
	self debugger announce: (DebugActionExecuted forDebuggingAction: self)
]

{ #category : #accessing }
DebugAction >> order [

	^ order ifNil: [ self defaultOrder ]
]

{ #category : #accessing }
DebugAction >> order: anInteger [

	order := anInteger
]

{ #category : #actions }
DebugAction >> postAction [

	self notifyDebuggingActionExecuted
]

{ #category : #actions }
DebugAction >> preAction [

	self notifyDebuggingActionAboutToExecute
]

{ #category : #actions }
DebugAction >> precondition [

	^ self needsValidation not or: [ self debugger okToChange ]
]

{ #category : #'accessing-context' }
DebugAction >> selectedContext [
	
	^ self debugger selectedContext
	
]

{ #category : #accessing }
DebugAction >> session [

	^ self debugger session
]

{ #category : #accessing }
DebugAction >> withSeparatorAfter [

	self needsSeparatorAfter: true
]
