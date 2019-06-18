"
I represent a setting of concrete instance of shortcut activation strategy, the annotation instance registered in the system.

I use redefinition mechanizm of class annotations to change the parameters of given shortcut activation. Currently I only modify key combination value, the actual shortcut value.

I mark redefined shortcut with special star (*) at the beginning of my label.

When I user reset value to default I revert redefined annotation instance:

	shortcutActivation revertRedefinedInstance

I am used in class side methods of CmdShortcutCommandActivation which settings browser nodes for all registered shortcut instances.

Internal Representation and Key Implementation Points.

    Instance Variables
	shortcutActivation:		<CmdShortuctCommandActivation>
"
Class {
	#name : #CmdShortcutSetting,
	#superclass : #SettingDeclaration,
	#instVars : [
		'shortcutActivation'
	],
	#category : #'Commander-Activators-Shortcut'
}

{ #category : #'shortcut editor' }
CmdShortcutSetting >> accept: aKeyCombination [ 

	shortcutActivation redefineBy: [  shortcutActivation keyCombination: aKeyCombination ].
	self updated
]

{ #category : #visitor }
CmdShortcutSetting >> acceptSettings: aVisitor [ 

	shortcutActivation isRedefined ifTrue: [ ^ aVisitor visitCmdShortcutSetting: self]
]

{ #category : #accessing }
CmdShortcutSetting >> default [

	^shortcutActivation isRedefined 
		ifTrue: [ shortcutActivation redefinedInstance ]
		ifFalse: [ shortcutActivation ]
]

{ #category : #accessing }
CmdShortcutSetting >> hasDefault [

	^shortcutActivation isRedefined
]

{ #category : #updating }
CmdShortcutSetting >> markRedefinedStatus [
	(label beginsWith: '*') ifTrue: [label := label allButFirst].
	
	shortcutActivation isRedefined ifTrue: [ label := '*', label ]
]

{ #category : #accessing }
CmdShortcutSetting >> realValue [

	^ shortcutActivation
]

{ #category : #accessing }
CmdShortcutSetting >> realValue: aShortcutActivation [

	shortcutActivation redefineBy: [
			shortcutActivation keyCombination: aShortcutActivation keyCombination].
		
	self updated
]

{ #category : #accessing }
CmdShortcutSetting >> setToDefault [

	shortcutActivation := shortcutActivation revertRedefinedInstanceIfAbsent: [
		self inform: 'Annotation not exists anymore'. 
		^self].
		
	self updated
]

{ #category : #accessing }
CmdShortcutSetting >> shortcut [
	^shortcutActivation keyCombination
]

{ #category : #accessing }
CmdShortcutSetting >> shortcutActivation [
	^ shortcutActivation
]

{ #category : #accessing }
CmdShortcutSetting >> shortcutActivation: aShortcutActivation [
	shortcutActivation := aShortcutActivation.
	
	label := shortcutActivation id.
	description := shortcutActivation commandClass comment.
	self markRedefinedStatus.
]

{ #category : #updating }
CmdShortcutSetting >> updated [

	self markRedefinedStatus.
	self triggerEvent: self privateChangeEvent with: self
]
