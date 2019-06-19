"
I describe how access and execute command using given shortcut.

Add me to commands using:

	YourCommand>>yourApplicationShortcutActivation
		<classAnnotation>
		^CmdShortcutCommandActivation by: $y meta for: YourAppContext

I also define standard shortcuts on class side for rename and remove commands:

- renamingFor: aToolContext
- removalFor: aToolContext

In addition I add to the settings browser the root group ""Shortcuts"" with all my registered instances.
So user can redefine default values in settings browser. I use class annotation redefinition mehanizm to support it. 
To reset all redefined values evaluate following expression:

	CmdShortcutCommandActivation revertRedefinedInstances. 

Internal Representation and Key Implementation Points.

    Instance Variables
	keyCombination:		<KKKeyCombination>
"
Class {
	#name : #CmdShortcutActivation,
	#superclass : #CmdCommandActivationStrategy,
	#instVars : [
		'keyCombination'
	],
	#category : #'Commander-Activators-Shortcut'
}

{ #category : #settings }
CmdShortcutActivation class >> buildSettingsFor: activations on: aBuilder [
	| sorted |
	sorted := activations sorted: [ :a :b | a commandClass name <= b commandClass name ].
	sorted do: [ :each |	each buildSettingNodeOn: aBuilder]
]

{ #category : #settings }
CmdShortcutActivation class >> buildSettingsOn: aBuilder [
	<systemsettings>
	(aBuilder group: #shortcuts)
		label: 'Shortcuts';
		description: 'All System Command Shortcuts';
		with: [
			self buildSettingsFor: self redefiningInstances on: aBuilder.
			self 
				buildSettingsFor: (self registeredInstances copyWithoutAll: self redefiningInstances)
				on: aBuilder.
		]
]

{ #category : #'instance creation' }
CmdShortcutActivation class >> by: aKeyCombination for: anAnnotationUser [  
	^(self for: anAnnotationUser)
		keyCombination: aKeyCombination 
]

{ #category : #'well known shortcuts' }
CmdShortcutActivation class >> removalFor: anAnnotationUser [  
	<classAnnotationDependency>
	
	^self by: $x meta for: anAnnotationUser  
]

{ #category : #'well known shortcuts' }
CmdShortcutActivation class >> renamingFor: anAnnotationUser [  
	<classAnnotationDependency>
	
	^self by: $r meta for: anAnnotationUser  
]

{ #category : #settings }
CmdShortcutActivation class >> settingInputWidgetForNode: aShortcutSetting [
	| catcherMorph theme |
	theme := UITheme builder.
	catcherMorph := KMCatcherMorph for: aShortcutSetting.
	^ theme newRow: {catcherMorph}
]

{ #category : #settings }
CmdShortcutActivation >> buildSettingNodeOn: aBuilder [
	| nodeBuilder |
	nodeBuilder := aBuilder 
		nodeClass: CmdShortcutSetting name: self id.
	nodeBuilder node item shortcutActivation: self.
	^nodeBuilder
]

{ #category : #accessing }
CmdShortcutActivation >> keyCombination [
	^ keyCombination
]

{ #category : #accessing }
CmdShortcutActivation >> keyCombination: anObject [
	keyCombination := anObject
]

{ #category : #printing }
CmdShortcutActivation >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(.
	keyCombination printOn: aStream.
	aStream nextPut: $).
]

{ #category : #'command execution' }
CmdShortcutActivation >> tryExecuteCommandInContext: aToolContext byEvents: anEventBuffer [
	
	| activator |
	(keyCombination matches: anEventBuffer) ifTrue: [ 
		(self isActiveInContext: aToolContext) ifFalse: [ ^false].
		
		(keyCombination matchesCompletely: anEventBuffer)
			ifTrue: [ KMBuffer uniqueInstance completeMatch.
				activator := self newActivatorFor: aToolContext.
				activator executeCommand]
			ifFalse: [ KMBuffer uniqueInstance partialMatch].
		^true].
	^false
]
