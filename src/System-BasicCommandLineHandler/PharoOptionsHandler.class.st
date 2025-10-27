"
I'm responsible to manage the default image options.

Usage: [<subcommand>] [--help] [--copyright] [--version] [--list] [ --no-quit ]
	--help       print this help message
	--copyright  print the copyrights
	--version    print the version for the image and the vm
	--list       list a description of all active command line handlers
	--no-quit    keep the image running without activating any other command line handler
	--deploymentPassword   if a password needs to be used by the user to launch the command
	--readWriteAccessMode, --readOnlyAccessMode, --writeOnlyAccessMode, --disabledAccessMode
	             specify disk access mode, read-write mode as default
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
	#name : 'PharoOptionsHandler',
	#superclass : 'Object',
	#instVars : [
		'arguments'
	],
	#classVars : [
		'ShouldIgnorePreferences'
	],
	#category : 'System-BasicCommandLineHandler',
	#package : 'System-BasicCommandLineHandler'
}

{ #category : 'accessing' }
PharoOptionsHandler class >> forArguments: arguments [

	^ self new
		arguments: arguments;
		yourself
]

{ #category : 'accessing' }
PharoOptionsHandler class >> handle: arguments [

	^ (self forArguments: arguments) handlePharoArguments
]

{ #category : 'accessing' }
PharoOptionsHandler class >> shouldIgnorePreferences [
	"If this variable is at true, command lines will not load preferences, even if the command line specify directly a preferences file. This is useful to deploy applications with proprietary code."

	^ ShouldIgnorePreferences ifNil: [ ShouldIgnorePreferences := false ]
]

{ #category : 'accessing' }
PharoOptionsHandler class >> shouldIgnorePreferences: anObject [
	ShouldIgnorePreferences := anObject
]

{ #category : 'accessing' }
PharoOptionsHandler >> arguments [
	^ arguments
]

{ #category : 'accessing' }
PharoOptionsHandler >> arguments: aCollection [ 
	
	arguments := aCollection 
]

{ #category : 'private - preferences' }
PharoOptionsHandler >> changePreferences [

	self isOmittingPreferences ifTrue: [ ^ self ].

	StartupPreferencesLoader default load: { self preferencesFile }
]

{ #category : 'accessing' }
PharoOptionsHandler >> filteredArguments [
	
	arguments ifEmpty: [ ^ arguments ].
	
	arguments first = self noDefaultPreferencesArgument 
		ifTrue: [ ^ arguments copyWithoutFirst ].
		
	(arguments first beginsWith: self preferencesFileArgument)
		ifTrue: [ ^ arguments copyWithoutFirst ].
		
	arguments first = '--interactive'
		ifTrue: [ ^ arguments copyWithoutFirst ].
		
	^ arguments
]

{ #category : 'execution' }
PharoOptionsHandler >> handlePharoArguments [
	
	"If the command line is configured to force the omission of preferences, we should skip them in any case. To ensure the command line works perfectly, we should still ensure that there is no unneeded parameters as 'preferences-file' of 'no-default-preferences'."

	self shouldIgnorePreferences
		ifFalse: [
			self isChangingPreferences
				ifTrue: [ self changePreferences ]
				ifFalse: [ self runPreferences ]
		].
	
	^ self filteredArguments
]

{ #category : 'private - preferences' }
PharoOptionsHandler >> isChangingPreferences [

	^ self isOverridingPreferences or: [ self isOmittingPreferences ]
]

{ #category : 'private - preferences' }
PharoOptionsHandler >> isOmittingPreferences [
	
	self arguments ifEmpty: [ ^ false ].

	^ self arguments first = self noDefaultPreferencesArgument
]

{ #category : 'private - preferences' }
PharoOptionsHandler >> isOverridingPreferences [

	self arguments ifEmpty: [ ^ false ].

	^ self arguments first beginsWith: self preferencesFileArgument
]

{ #category : 'accessing' }
PharoOptionsHandler >> noDefaultPreferencesArgument [

	^ '--no-default-preferences'
]

{ #category : 'accessing' }
PharoOptionsHandler >> preferencesFile [

	| preferencesFileArgument |
	preferencesFileArgument := self arguments first.
	^  (preferencesFileArgument splitOn: '=') second asFileReference
]

{ #category : 'accessing' }
PharoOptionsHandler >> preferencesFileArgument [

	^ '--preferences-file'
]

{ #category : 'private - preferences' }
PharoOptionsHandler >> runPreferences [

	Smalltalk at: #SystemSettingsPersistence ifPresent: [:persistence |
		persistence resumeSystemSettings].
	Smalltalk at: #StartupPreferencesLoader ifPresent: [:loader |
		loader default loadFromDefaultLocations ]
]

{ #category : 'accessing' }
PharoOptionsHandler >> shouldIgnorePreferences [
	^ self class shouldIgnorePreferences
]
