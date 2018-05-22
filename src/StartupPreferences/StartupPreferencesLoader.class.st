"
StartupPreferencesLoader searches for and executes .st files from certain locations such as Library/Preferences/pharo on Mac OS X.  

StartupPreferencesLoader looks within such locations for a 'pharo' folder. This contains the startup scripts common to all versions of Pharo, and also optionally a folder per Pharo version holding startup scripts suitable for that version only.  So a typical directory layout might be...

.../some/folders/pharo/Content/Resources/pharo.image.
.../some/folders/pharo/Content/Resources/startup.st
.../some/folders/.config/pharo/author.st
.../some/folders/.config/pharo/useSharedCache.st
.../some/folders/.config/pharo/1.4/mystartupFor14only.st
.../some/folders/.config/pharo/2.0/mystartupFor20only.st

(**Note however that '.config' is an invalid filename on Windows, so '..config' is used instead)

To know the real values for you...
Print the result of ""StartupPreferencesLoader preferencesGeneralFolder"" which holds the startup scripts common to all versions of Pharo.
Print the result of ""StartupPreferencesLoader preferencesVersionFolder"" which holds the startup scripts specific to the version of the current image.

-----------


StartupPreferencesLoader example

will define a script sample startup.st in your unix root on unix 

Its contents is 

StartupPreferencesLoader default executeAtomicItems: {
	StartupAtomicItem name: 'Open Help' code: 'Workspace openContents: ''Here is just an example of how to use the StartupPreferencesLoader.
I should only be displayed once.
	
You can also see StartupPreferencesLoader class>>#example'' label: ''Help''' isSingleton: true.
	StartupAtomicItem name: 'Open Workspace' code: 'Workspace openContents: ''I should be displayed each time'''.
}

EXAMPLE 1
============

	"" an example of script generation ""
	| item1 item2 |
	item1 := StartupAction name: 'Open Help' code: 'Smalltalk tools workspace openContents: ''Here is just an example of how to use the StartupPreferencesLoader.
I should only be displayed once.
	
 You can also see StartupPreferencesLoader class>>#example'' label: ''Help''' runOnce: true.
	item2 := StartupAction name: 'Open Workspace' code:  [ Workspace openContents: 'I should be displayed each time' ].
	StartupPreferencesLoader default addAtStartupInGeneralPreferenceFolder: {item1. item2}.
	
	StartupPreferencesLoader default loadFromDefaultLocations.
	StartupPreferencesLoader default loadFromDefaultLocations.

EXAMPLE 2
============

	"" it's my personal script provided as example""
	| items |
	items := OrderedCollection new.
	items add: (StartupAction name: 'Set the Author' code: [ Author fullName: Author fullName printString ]). ""replace it by your name""
	
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: items named: 'author.st'.
	items removeAll.
	
	items add: (StartupAction name: 'Debugger option' code: [ Smalltalk tools debugger alwaysOpenFullDebugger: true ]).
	items add: (StartupAction name: 'Dragging Option' code: [ UITheme defaultSettings fastDragging: true ]).
	items add: (StartupAction name: 'Dialog Auto Accept' code: [ TextEditorDialogWindow autoAccept: true ]).
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: items named: 'settings.st'.
	items removeAll.
	
	items add: (StartupAction name: 'Fonts option' code: [ FreeTypeSystemSettings loadFt2Library: true.	
	FreeTypeFontProvider current updateFromSystem.
	StandardFonts defaultFont: (LogicalFont familyName: 'Lucida Grande' pointSize: 10) forceNotBold.
	GraphicFontSettings resetAllFontToDefault.
	StandardFonts codeFont: (LogicalFont familyName: 'Consolas' pointSize: 10).] runOnce: true).
	
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: items named: 'fonts.st'.
	items removeAll.
	StartupPreferencesLoader default addAtStartupInImageDirectory: items.
	
	StartupPreferencesLoader default loadFromDefaultLocations.

EXAMPLE 3
============

	| items |
	items := OrderedCollection new.
	
	items add: (StartupAction name: 'General Preferences for all Pharo versions' code: [ 
		FileStream stdout lf; nextPutAll: 'Setting general preferences for all Pharo versions'; lf.
		FileStream stdout lf; nextPutAll: 'Finished'; lf.
		 ]).
	StartupPreferencesLoader default addAtStartupInGeneralPreferenceFolder: items named: 'generalSettings.st'.
	
	items removeAll.
	items add: (StartupAction name: 'Settings' code: [ 
		FileStream stdout lf; nextPutAll: 'Setting general preferences for Pharo 2.0'; lf.
		FileStream stdout lf; nextPutAll: 'Finished'; lf.
		 ]).
	StartupPreferencesLoader default addAtStartupInPreferenceVersionFolder: items named: 'settings.st'.
	
	items removeAll.
	items add: (StartupAction name: 'Image diretory' code: [ 
		FileStream stdout lf; nextPutAll: 'Setting preferences for image directory'; lf.
		FileStream stdout lf; nextPutAll: 'Finished'; lf.
		 ]).
	StartupPreferencesLoader default addAtStartupInImageDirectory: items 
"
Class {
	#name : #StartupPreferencesLoader,
	#superclass : #Object,
	#instVars : [
		'errors',
		'actions'
	],
	#classVars : [
		'AllowStartupScript',
		'UniqueInstance'
	],
	#category : #StartupPreferences
}

{ #category : #accessing }
StartupPreferencesLoader class >> allowStartupScript [

	^ AllowStartupScript ifNil: [ AllowStartupScript := true ]
]

{ #category : #accessing }
StartupPreferencesLoader class >> allowStartupScript: aBoolean [

	AllowStartupScript := aBoolean
]

{ #category : #cleanup }
StartupPreferencesLoader class >> cleanUp: aggressive [
	self reset
]

{ #category : #'instance creation' }
StartupPreferencesLoader class >> default [

	^ UniqueInstance ifNil: [ UniqueInstance := super basicNew initialize ]
]

{ #category : #'instance creation' }
StartupPreferencesLoader class >> new [

	^ self error: 'use #default instead'
]

{ #category : #private }
StartupPreferencesLoader class >> preferencesGeneralFolder [

	^ [ FileLocator preferences asFileReference / 'pharo' ]
		on: Error
		do: [ :ex |
			"Non NB-VMs will fail with a generic error"		
			StartupPreferencesLoadingError signal ]
]

{ #category : #private }
StartupPreferencesLoader class >> preferencesVersionFolder [

	^ self preferencesGeneralFolder 
		ifNotNil: [ :folder | folder / SystemVersion current dottedMajorMinor ]
]

{ #category : #'instance creation' }
StartupPreferencesLoader class >> reset [

	UniqueInstance := nil
]

{ #category : #private }
StartupPreferencesLoader class >> startupGeneralPreferencesFolderScriptName [

	^ 'startupPharo.st'
]

{ #category : #setting }
StartupPreferencesLoader class >> startupPreferencesSettingsOn: aBuilder [
	<systemsettings>
	
	(aBuilder setting: #allowStartupScript)
		label: 'Allow the preferences to be loaded at startup';
		parent: #pharoSystem;
		target: self;
		description: 'If true, the preferences will be loaded' .
]

{ #category : #private }
StartupPreferencesLoader class >> startupPreferencesVersionFolderScriptName [

	^ 'startupPharo', SystemVersion current dottedMajorMinor,'.st'
]

{ #category : #private }
StartupPreferencesLoader class >> startupScriptName [

	^ 'startup.st'
]

{ #category : #accessing }
StartupPreferencesLoader >> actions [

	^ actions
]

{ #category : #accessing }
StartupPreferencesLoader >> add: anAction [
	| action |
	action := actions at: anAction name ifAbsentPut: [ anAction ].
	"make sure the possible existing action ueses new code or
	changed runOnce-behavior"
	action code: anAction code.
	action runOnce: anAction runOnce.
	^ action
]

{ #category : #private }
StartupPreferencesLoader >> addAtStartup: aCollection inDirectory: aFileReference named: fileName [

	| scriptFile |
	aFileReference ensureCreateDirectory.
	scriptFile := aFileReference / fileName.
	FileStream forceNewFileNamed: scriptFile fullName do: [ :stream | stream nextPutAll: (self buildStreamFor: aCollection) ].

]

{ #category : #'script generation' }
StartupPreferencesLoader >> addAtStartupInGeneralPreferenceFolder: aCollection [ 
	self 
		addAtStartup: aCollection 
		inDirectory: self preferencesGeneralFolder 
		named: self class startupGeneralPreferencesFolderScriptName
]

{ #category : #'script generation' }
StartupPreferencesLoader >> addAtStartupInGeneralPreferenceFolder: aCollection named: fileName [
	self 
		addAtStartup: aCollection 
		inDirectory: self preferencesGeneralFolder 
		named: fileName
]

{ #category : #'script generation' }
StartupPreferencesLoader >> addAtStartupInImageDirectory:  aCollection [
	self 
		addAtStartup: aCollection 
		inDirectory: FileSystem workingDirectory 
		named: self class startupScriptName

]

{ #category : #'script generation' }
StartupPreferencesLoader >> addAtStartupInPreferenceVersionFolder: aCollection [ 
	self 
		addAtStartup: aCollection 
		inDirectory: self preferencesVersionFolder 
		named: self class startupPreferencesVersionFolderScriptName
]

{ #category : #'script generation' }
StartupPreferencesLoader >> addAtStartupInPreferenceVersionFolder: aCollection named: fileName [
	self 
		addAtStartup: aCollection 
		inDirectory: self preferencesVersionFolder 
		named: fileName
]

{ #category : #private }
StartupPreferencesLoader >> buildStreamFor: aCollection [
	^ String
		streamContents:
			[ :stream | 
			stream
				nextPutAll: 'StartupPreferencesLoader default executeAtomicItems: {';
				crlf.
			aCollection
				do: [ :item | 
					stream
						tab;
						nextPutAll: item storeString;
						nextPut: $.;
						crlf ].
			stream
				nextPut: $};
				nextPut: $.;
				crlf;
				crlf ]
]

{ #category : #cleaning }
StartupPreferencesLoader >> cleanSavedActionsAndErrors [
	actions := Dictionary new. 
	errors := OrderedCollection new. 
]

{ #category : #accessing }
StartupPreferencesLoader >> errors [

	^ errors
]

{ #category : #'code execution' }
StartupPreferencesLoader >> executeAtomicItems: aCollection [
	"No sender but send from the generated script"

	'Load Startup Preferences'
		displayProgressFrom: 0 to: aCollection size
		during: [:bar | 
			aCollection doWithIndex: [:item :index | 
				bar current: index.
				(self add: item) execute ]]
]

{ #category : #initialization }
StartupPreferencesLoader >> initialize [

	super initialize.
	actions := Dictionary new. 
	errors := OrderedCollection new. 
]

{ #category : #private }
StartupPreferencesLoader >> load: aCollection [
	errors removeAll.
	aCollection do: [:file | file fileIn ].
]

{ #category : #'startup loading' }
StartupPreferencesLoader >> loadFromDefaultLocations [
	self class allowStartupScript ifFalse: [ ^ self ].
	
	[ self load: self retrieveFilesStream ]
		on: StartupPreferencesLoadingError
		do: [ :ex | ex return ]
]

{ #category : #utilities }
StartupPreferencesLoader >> preferencesGeneralFolder [

	^ self class preferencesGeneralFolder.
]

{ #category : #utilities }
StartupPreferencesLoader >> preferencesVersionFolder [

	^ self class preferencesVersionFolder.
]

{ #category : #cleaning }
StartupPreferencesLoader >> removeAllScriptsAndCleanSavedActions [
	self removeAllScriptsFromAllDirectories.
	self cleanSavedActionsAndErrors.
	
]

{ #category : #cleaning }
StartupPreferencesLoader >> removeAllScriptsFromAllDirectories [
	self removeScriptFromImageDirectory.
	self removeAllScriptsFromPreferencesVersionDirectory.
	self removeAllScriptsFromGeneralPreferencesDirectory.

]

{ #category : #cleaning }
StartupPreferencesLoader >> removeAllScriptsFromDirectory: aDirectory [
	aDirectory deleteAllChildren
]

{ #category : #cleaning }
StartupPreferencesLoader >> removeAllScriptsFromGeneralPreferencesDirectory [
	self removeAllScriptsFromDirectory: self preferencesGeneralFolder.
	
	

]

{ #category : #cleaning }
StartupPreferencesLoader >> removeAllScriptsFromPreferencesVersionDirectory [
	self removeAllScriptsFromDirectory: self preferencesVersionFolder.


]

{ #category : #cleaning }
StartupPreferencesLoader >> removeScriptFromImageDirectory [
	(FileSystem workingDirectory / self class startupScriptName) delete.

]

{ #category : #'startup loading' }
StartupPreferencesLoader >> retrieveFilesStream [

	^ StartupPreferencesHandler new perform
]
