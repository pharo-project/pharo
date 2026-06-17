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
MissionControlShortcutsCategory >> missionControlDismiss [

	<shortcut>
	^ KMKeymap shortcut: Character arrowDown meta action: [ WorldMorph allInstances first dismissMissionControl ]
]

{ #category : 'keymaps' }
MissionControlShortcutsCategory >> missionControlToggle [

	<shortcut>
	^ KMKeymap shortcut: Character arrowUp meta action: [ WorldMorph allInstances first missionControl ]
]
