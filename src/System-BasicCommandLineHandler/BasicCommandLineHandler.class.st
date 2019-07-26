"
I'm a command line handler who is not aware of any UI or Tools present in the system.
I'm used as base handler for loading executing stripped images (kernel images, for example). 

My usage is the same as PharoCommandLineHandler, see his comment as better explanation.

I also implement a way to password-protect command lines. 

The password will not be saved as in clear. It will be hash using pepper and iterations.

The pepper of a hash is a fix string happened to a password to increase the difficulty of finding the password. Also, we hash multiple times (iterations) to increase the strenght of the protection.

If you wish to define ""application"" command lines who do not need a password protection, implement the method #requirePasswordInDeployment on the class side to return false.

Examples of password protection
----------------

""Enable password protection""
CommandLinePasswordManager protectCommandLinesByPasswordWith: 'PharoPassword'

""You can also customize the pepper and number of iterations for the hashing of the password.""
CommandLinePasswordManager protectCommandLinesByPasswordWith: 'PharoPassword' pepper: 'SomePepper' numberOfHashIterations: 10

""Remove password protection""
CommandLinePasswordManager removePasswordProtection

How to specify the password
----------------

Command line without password:
	Pharo.exe Pharo.image eval ""1+1""

Command line with password:
	Pharo.exe Pharo.image eval ""1+1"" --deploymentPassword=Test
"
Class {
	#name : #BasicCommandLineHandler,
	#superclass : #CommandLineHandler,
	#category : #'System-BasicCommandLineHandler-Base'
}

{ #category : #'password-protection' }
BasicCommandLineHandler class >> commandLinePasswordManager [
	^ CommandLinePasswordManager current
]

{ #category : #accessing }
BasicCommandLineHandler class >> commandName [
	^ ''
]

{ #category : #'handler selection' }
BasicCommandLineHandler class >> description [
	^ 'basic responsible for the default options and activating other commands'
]

{ #category : #'class initialization' }
BasicCommandLineHandler class >> initialize [ 
	SessionManager default 
		registerSystemClassNamed: self name
]

{ #category : #'handler selection' }
BasicCommandLineHandler class >> isResponsibleFor: aCommandLine [
	"I do not match ever, because my activation is manual"
	^ false
]

{ #category : #accessing }
BasicCommandLineHandler class >> priority [ 
	"Lowest priority"
	^ Float infinity negated
]

{ #category : #'system startup' }
BasicCommandLineHandler class >> startUp: isImageStarting [
	"only handle when lauching a new image"
	isImageStarting ifFalse: [ ^ self ].

	Smalltalk session 
		addDeferredStartupAction: [ self new activate ]
]

{ #category : #activation }
BasicCommandLineHandler >> activate [
	[ [ self handleArgument: (self arguments ifEmpty: [ '' ] ifNotEmpty: [ :arguments | arguments first ]) ]
		on: Exit
		do: [ :exit | 
			"If the command line is protected by password, we just exit the image because in non-headless mode the handleExit will let the image open. If the password protection is enabled, it is to avoid to let the access to the image."
			self class commandLinePasswordManager hasPasswordSet
				ifTrue: [ Smalltalk snapshot: false andQuit: true ].
			^ self handleExit: exit ] ] forkAt: Processor userSchedulingPriority named: 'CommandLine handler process'
]

{ #category : #activation }
BasicCommandLineHandler >> activateSubCommand: aCommandLinehandler [
	[ aCommandLinehandler activateWith: commandLine ] on: Exit do: [ :exit |
		^ self handleExit: exit for: aCommandLinehandler ].
	"the return value of this method is used to check if the subcommand was successfull"
	^ aCommandLinehandler
]

{ #category : #password }
BasicCommandLineHandler >> checkPassword [
	self
		optionAt: 'deploymentPassword'
		ifPresent: [ :password | self handlePassword: password ]
		ifAbsent: [ self stderr << 'This command line is protected by a password'.
			Smalltalk snapshot: false andQuit: true ].
	commandLine := commandLine copyWithoutPassword
]

{ #category : #commands }
BasicCommandLineHandler >> copyright [
	self stdout 
		nextPutAll: Smalltalk licenseString; cr.
	self quit.
]

{ #category : #commands }
BasicCommandLineHandler >> default [
	^ self 
		help;
		exitSuccess
]

{ #category : #commands }
BasicCommandLineHandler >> disabledAccessMode [

	"just eat the argument because it was processed with by the SessionAccessModeResolver before"
]

{ #category : #commands }
BasicCommandLineHandler >> error [
	self arguments size = 1
		ifTrue:  [
			(self arguments first beginsWith: '-')
				ifTrue:  [ self << 'Invalid option: ']
				ifFalse: [ self << 'Invalid sub command: ']]
		ifFalse: [ self << 'Invalid arguments: "'].
		
	(self << (' ' join: self arguments)) 
		nextPutAll: '"'; cr.
		
	self printHelp.
	
	self exitFailure.
]

{ #category : #activation }
BasicCommandLineHandler >> handleArgument: aString [

	"give priority to subcommands"
	self handleSubcommand == self
		ifFalse: [ ^ self ].

	"check for default options"
	aString ifEmpty: [ ^ self default ].
	
	aString = '--version'
		ifTrue: [ ^ self version ].
		
	aString = '--help'
		ifTrue: [ ^ self help ].
		
	aString = '--list'
		ifTrue: [ ^ self list ].
		
	aString = '--copyright'
		ifTrue: [ ^ self copyright ].
		
	aString = '--no-quit'
		ifTrue: [ ^ self noQuit ].

	aString = '--interactive'
		ifTrue: [ ^ self noQuit ].

	aString = '--readWriteAccessMode'
		ifTrue: [ ^ self readWriteAccessMode ].
	aString = '--readOnlyAccessMode'
		ifTrue: [ ^ self readOnlyAccessMode ].
	aString = '--writeOnlyAccessMode'
		ifTrue: [ ^ self writeOnlyAccessMode ].
	aString = '--disabledAccessMode'
		ifTrue: [ ^ self disabledAccessMode ].
	
	"none of the previous options matched hence we output an error message"
	self error.
]

{ #category : #activation }
BasicCommandLineHandler >> handleEmptyArguments [
	Smalltalk isHeadless
		ifFalse: [ ^ self ].
	
	self help.
]

{ #category : #activation }
BasicCommandLineHandler >> handleExit: exit [ 
	^ self handleExit: exit  for: self
]

{ #category : #activation }
BasicCommandLineHandler >> handleExit: exit for: aCommandLinehandler [

	Smalltalk isInteractive 
		ifFalse: [ ^ exit pass ].
		
	exit isSuccess 
		ifFalse: [ ^ Error signal: exit messageText ].
	
	self inform: aCommandLinehandler name, ' successfully finished'.
		
	"for failing subcommands return self which is used to check if the subcommand failed"
	exit isSuccess 
		ifTrue: [ ^ aCommandLinehandler ].
]

{ #category : #password }
BasicCommandLineHandler >> handlePassword: password [
	(self class commandLinePasswordManager isMatchingPassword: password) ifTrue: [ ^ self ].
	
	self stderr << 'ERROR: Wrong password.'.
	Smalltalk snapshot: false andQuit: true
]

{ #category : #activation }
BasicCommandLineHandler >> handleSubcommand [
	"check if there is an active subcommand"

	| handlers |
	handlers := self selectHandlers.

	"If there is no handler, use myself"
	handlers ifEmpty: [ ^ self ].
	
	(self requirePasswordFor: handlers first)
		ifTrue: [ self checkPassword ].
		
	^ self activateSubCommand: handlers first
]

{ #category : #initialization }
BasicCommandLineHandler >> initialize [
	super initialize.
	commandLine := CommandLineArguments new
]

{ #category : #commands }
BasicCommandLineHandler >> list [
	| handlers shortNames maxShortNameSize |
	self stdout 
		nextPutAll: 'Currently installed Command Line Handlers:'; lf.

	handlers := self allHandlers reject: [ :cls | 
			cls includesBehavior: BasicCommandLineHandler ].
	shortNames := handlers collect: #commandName.
	maxShortNameSize := (shortNames detectMax: [ :name|  name size ]) size.
	
	handlers do: [ :handler|
		self stdout 
			nextPutAll: '    ';
			nextPutAll: (handler commandName padRightTo: maxShortNameSize); 
			nextPutAll: '    ';
			nextPutAll: (handler description ifNil: [ '--']); lf].
	self quit.
]

{ #category : #commands }
BasicCommandLineHandler >> noQuit [
	"Nothing to be done, unlike the other commands the image continues running"
]

{ #category : #commands }
BasicCommandLineHandler >> readOnlyAccessMode [

	"just eat the argument because it was processed with by the SessionAccessModeResolver before"
]

{ #category : #commands }
BasicCommandLineHandler >> readWriteAccessMode [

	"just eat the argument because it was processed with by the SessionAccessModeResolver before"
]

{ #category : #password }
BasicCommandLineHandler >> requirePasswordFor: handler [
	"If the password hash is nil, no password is set and we can just ignore it."

	^ handler requireDeploymentPassword
		and: [ self class commandLinePasswordManager hasPasswordSet ]
]

{ #category : #private }
BasicCommandLineHandler >> selectHandlers [
	| handlers |
	handlers := (self subCommandsRoot selectHandlersFor: commandLine) copyWithout: self class.
	handlers := handlers sort: [ :a :b | a priority >= b priority ].
	^ handlers
]

{ #category : #private }
BasicCommandLineHandler >> selectedHandler [
	^ self selectHandlers 
		ifEmpty: [ nil ] 
		ifNotEmpty: [ :handlers | handlers first ]
]

{ #category : #private }
BasicCommandLineHandler >> subCommandsRoot [
	^ CommandLineHandler
]

{ #category : #commands }
BasicCommandLineHandler >> version [
	self stdout 
		nextPutAll: 'Image: '; print: SystemVersion current; cr;
		nextPutAll: 'VM:    '; nextPutAll: Smalltalk vm version; cr.
	self quit.
]

{ #category : #commands }
BasicCommandLineHandler >> writeOnlyAccessMode [

	"just eat the argument because it was processed with by the SessionAccessModeResolver before"
]
