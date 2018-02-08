"
Usage: [<subcommand>] [--help] [--copyright] [--version] [--list] [ --no-quit ]
	--help       print this help message
	--copyright  print the copyrights
	--version    print the version for the image and the vm
	--list       list a description of all active command line handlers
	--no-quit    keep the image running without activating any other command line handler
	<subcommand> a valid subcommand in --list
	
	Preference File Modification:
	--preferences-file   load the preferences from the given <FILE>
	--no-default-preferences    do not load any preferences from the default locations
	
Documentation:
A PharoCommandLineHandler handles default command line arguments and options.
The PharoCommandLineHandler is activated before all other handlers. 
It first checks if another handler is available. If so it will activate the found handler.
"
Class {
	#name : #PharoCommandLineHandler,
	#superclass : #BasicCommandLineHandler,
	#classVars : [
		'ForcePreferencesOmission'
	],
	#category : #'System-CommandLineHandler'
}

{ #category : #'instance creation' }
PharoCommandLineHandler class >> activateWith: aCommandLine [	
	"Make sure that the PharoCommandLineHandler starts at the top of the stack in the main UI thread."
	UIManager default defer:  [ 
		super activateWith: aCommandLine ]
]

{ #category : #accessing }
PharoCommandLineHandler class >> description [
	^ 'responsible for the default options and activating other commands'
]

{ #category : #accessing }
PharoCommandLineHandler class >> forcePreferencesOmission [
	"If this variable is at true, command lines will not load preferences, even if the command line specify directly a preferences file. This is useful to deploy applications with proprietary code."

	^ ForcePreferencesOmission ifNil: [ ForcePreferencesOmission := false ]
]

{ #category : #accessing }
PharoCommandLineHandler class >> forcePreferencesOmission: anObject [
	ForcePreferencesOmission := anObject
]

{ #category : #'handler selection' }
PharoCommandLineHandler class >> isResponsibleFor: aCommandLine [
	"I do not match ever, because my activation is manual"

	^ true
]

{ #category : #accessing }
PharoCommandLineHandler class >> priority [ 
	"Highest priority"
	^ Float infinity
]

{ #category : #activation }
PharoCommandLineHandler >> activate [
	"If the command line is configured to force the omission of preferences, we should skip them in any case. To ensure the command line works perfectly, we should still ensure that there is no unneeded parameters as 'preferences-file' of 'no-default-preferences'."

	self forcePreferencesOmission
		ifTrue: [ self isChangingPreferences ifTrue: [ self copySubcommand ] ]
		ifFalse: [ 
			self isChangingPreferences
				ifTrue: [ self changePreferences ]
				ifFalse: [ self runPreferences ] 
		].
	
	^ super activate
]

{ #category : #'private - preferences' }
PharoCommandLineHandler >> changePreferences [
	| preferenceFile |
	self isOmittingPreferences
		ifTrue: [ self copySubcommand.
			^ self ].
		
	preferenceFile := (self optionAt: 'preferences-file') asFileReference.
	self copySubcommand.
	
	StartupPreferencesLoader default load: {preferenceFile}
]

{ #category : #'private - preferences' }
PharoCommandLineHandler >> copySubcommand [
	commandLine := commandLine copySubcommand
]

{ #category : #commands }
PharoCommandLineHandler >> default [
	Smalltalk isHeadless 
		ifFalse: [ ^ self noQuit ].
	
	^ super default
]

{ #category : #accessing }
PharoCommandLineHandler >> forcePreferencesOmission [
	^ self class forcePreferencesOmission
]

{ #category : #'private - preferences' }
PharoCommandLineHandler >> isChangingPreferences [

	^ self isOverridingPreferences or: [ self isOmittingPreferences ]
]

{ #category : #'private - preferences' }
PharoCommandLineHandler >> isOmittingPreferences [

	^ self hasOption: 'no-default-preferences'
]

{ #category : #'private - preferences' }
PharoCommandLineHandler >> isOverridingPreferences [

	^ self hasOption: 'preferences-file'
]

{ #category : #'private - preferences' }
PharoCommandLineHandler >> runPreferences [

	Smalltalk at: #SystemSettingsPersistence ifPresent: [:persistence | 
		persistence resumeSystemSettings].
	Smalltalk at: #StartupPreferencesLoader ifPresent: [:loader |
		loader default loadFromDefaultLocations ].

]
