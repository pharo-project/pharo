"
Global keyboard shortcuts for Mission Control.
Provides Cmd+Up, Cmd+Down and F3 shortcuts that work from any morph,
following the same approach as ToolShortcutsCategory (Cmd+O+P, etc).
"
Class {
	#name : 'MissionControlShortcutsCategory',
	#superclass : 'KMCategory',
	#category : 'Morphic-Widgets-MissionControl',
	#package : 'Morphic-Widgets-MissionControl'
}

{ #category : 'testing' }
MissionControlShortcutsCategory class >> isGlobalCategory [

	^ true
]

{ #category : 'keymaps' }
MissionControlShortcutsCategory >> isAlreadyOpen: world [
	
	^ world submorphs anySatisfy: [ :m | m isKindOf: MissionControlMorph ]
]

{ #category : 'keymaps' }
MissionControlShortcutsCategory >> missionControlDismiss [

	<shortcut>
	^ KMKeymap shortcut: Character arrowDown meta action: [ :target :morph :event | | world |
		world := WorldMorph allInstances first.
		(self isAlreadyOpen: world) ifTrue: [ world dismissMissionControl ]
			ifFalse: [ world missionControlWithSameType ]
	]
]

{ #category : 'keymaps' }
MissionControlShortcutsCategory >> missionControlToggle [

	<shortcut>
	^ KMKeymap shortcut: Character arrowUp meta action: [
		WorldMorph allInstances first missionControl
	]
]
