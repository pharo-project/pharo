"
ToolShortcuts defines some global keyboard shortcuts to open tools for workspaces, system browser, monticello browser, settings, transcript and also image saving. 
"
Class {
	#name : 'ToolShortcutsCategory',
	#superclass : 'KMCategory',
	#category : 'Tool-Base-Keymapping',
	#package : 'Tool-Base',
	#tag : 'Keymapping'
}

{ #category : 'testing' }
ToolShortcutsCategory class >> isGlobalCategory [

	^ true
]

{ #category : 'keymaps' }
ToolShortcutsCategory >> openPlayground [

	<shortcut>
	^ KMKeymap 
		shortcut: PharoShortcuts current openPlaygroundShortcut 
		action: [ (self  tools toolNamed: #workspace) open ]
]

{ #category : 'keymaps' }
ToolShortcutsCategory >> openSettings [
	<shortcut>
	^ KMKeymap shortcut: PharoShortcuts current openSettingsShortcut action: [ SettingBrowser open ]
]

{ #category : 'keymaps' }
ToolShortcutsCategory >> openSystemBrowser [

	<shortcut>
	^ KMKeymap
		  shortcut: PharoShortcuts current openSystemBrowserShortcut
		  action: [ (self tools toolNamed: #browser) open ]
]

{ #category : 'keymaps' }
ToolShortcutsCategory >> openTranscript [

	<shortcut>
	^ KMKeymap
		  shortcut: PharoShortcuts current openTranscriptShortcut
		  action: [ (self tools toolNamed: #transcript) open ]
]

{ #category : 'keymaps' }
ToolShortcutsCategory >> openUnitTestRunner [

	<shortcut>
	^ KMKeymap
		  shortcut: PharoShortcuts current openTestRunnerShortcut
		  action: [ (self tools toolNamed: #testRunner) open ]
]

{ #category : 'keymaps' }
ToolShortcutsCategory >> saveImage [
	<shortcut>
	^ KMKeymap
		shortcut: PharoShortcuts current saveImageShortcut
		action: [
			(Smalltalk snapshot: true andQuit: false) hasSavedSuccessfully
				ifTrue: [ self inform: 'Image saved' ] ]
]
