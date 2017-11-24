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

	self isChangingPreferences
		ifTrue: [ self changePreferences ]
		ifFalse: [ self runPreferences ].
		
	^ super activate.
]

{ #category : #'private - preferences' }
PharoCommandLineHandler >> changePreferences [	
	| preferenceFile |
	
	self isOmittingPreferences
		ifTrue: [ 
			commandLine := commandLine copySubcommand.
			^ self ].
		
	preferenceFile := (self optionAt: 'preferences-file') asFileReference.
	commandLine := commandLine copySubcommand.
	
	StartupPreferencesLoader default load: { preferenceFile }.
]

{ #category : #commands }
PharoCommandLineHandler >> default [
	Smalltalk isHeadless 
		ifFalse: [ ^ self noQuit ].
	
	^ super default
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
