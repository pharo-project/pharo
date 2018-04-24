"
ToolShortcuts defines some global keyboard shortcuts to open tools for workspaces, system browser, monticello browser, settings, transcript and also image saving. 
"
Class {
	#name : #ToolShortcutsCategory,
	#superclass : #KMCategory,
	#category : #'Tool-Base'
}

{ #category : #testing }
ToolShortcutsCategory class >> isGlobalCategory [

	^ true 
]

{ #category : #keymaps }
ToolShortcutsCategory >> openMonticelloPackageBrowser [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openMonticielloShortcut action: [ Smalltalk tools openMonticelloBrowser ]
]

{ #category : #keymaps }
ToolShortcutsCategory >> openSettings [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openSettingsShortcut action: [ SettingBrowser open ]
]

{ #category : #keymaps }
ToolShortcutsCategory >> openSystemBrowser [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openSystemBrowserShortcut action: [ Smalltalk tools openClassBrowser ]
]

{ #category : #keymaps }
ToolShortcutsCategory >> openTranscript [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openTranscriptShortcut action: [ Smalltalk tools openTranscript ]
]

{ #category : #keymaps }
ToolShortcutsCategory >> openUnitTestRunner [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openTestRunnerShortcut action: [ Smalltalk tools openTestRunner ]
]

{ #category : #keymaps }
ToolShortcutsCategory >> openWorkspace [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openPlaygroundShortcut action: [ Smalltalk tools workspace open ]
]

{ #category : #keymaps }
ToolShortcutsCategory >> saveImage [
	<shortcut>
	^ KMKeymap
		shortcut: PharoShortcuts current saveImageShortcut
		action: [ 
			(Smalltalk snapshot: true andQuit: false)
				ifFalse: [ UIManager default inform: 'Image saved' ] ]
]
