"
I provide a management of the password that is needed to execute the commands throught the command line handler. Use public API to define a new password a remove the current one.

The password will not be saved as in clear. It will be hash using pepper and iterations.

The pepper of a hash is a fix string happened to a password to increase the difficulty of finding the password. Also, we hash multiple times (iterations) to increase the strenght of the protection.

If you wish to define ""application"" command lines who do not need a password protection, implement the method #requirePasswordInDeployment on the class side to return false.

Examples of password protection
----------------

""Enable password protection""
BasicCommandLineHandler  commandLinePasswordManager protectCommandLinesByPasswordWith: 'PharoPassword'

""You can also customize the pepper and number of iterations for the hashing of the password.""
BasicCommandLineHandler  commandLinePasswordManager protectCommandLinesByPasswordWith: 'PharoPassword' pepper: 'SomePepper' numberOfHashIterations: 10

""Remove password protection""
BasicCommandLineHandler  commandLinePasswordManager removePasswordProtection
"
Class {
	#name : #CommandLinePasswordManager,
	#superclass : #Object,
	#instVars : [
		'hashingPepper',
		'numberOfHashIterations',
		'passwordHash'
	],
	#classVars : [
		'Instance'
	],
	#category : #'System-BasicCommandLineHandler'
}

{ #category : #accessing }
CommandLinePasswordManager class >> current [
	^ Instance ifNil: [ self new ]
]

{ #category : #accessing }
CommandLinePasswordManager class >> defaultHashingPepper [
	^ 'Pharo'
]

{ #category : #accessing }
CommandLinePasswordManager class >> defaultNumberOfHashIterations [
	^ 5
]

{ #category : #public }
CommandLinePasswordManager class >> protectCommandLinesByPasswordWith: aString [
	"This method enables the password protection of command line. All command lines returning true to #requireDeploymentPassword ask a password to be executed. This is usefull in deployment mode of private applications."
	
	self protectCommandLinesByPasswordWith: aString pepper: nil numberOfHashIterations: nil
]

{ #category : #public }
CommandLinePasswordManager class >> protectCommandLinesByPasswordWith: aString pepper: anotherString [
	"This method enables the password protection of command line. All command lines returning true to #requireDeploymentPassword ask a password to be executed. This is usefull in deployment mode of private applications.
		This method accepts a custom pepper for the password hashing. See https://en.wikipedia.org/wiki/Pepper_(cryptography) for more information."

	self protectCommandLinesByPasswordWith: aString pepper: anotherString numberOfHashIterations: nil
]

{ #category : #public }
CommandLinePasswordManager class >> protectCommandLinesByPasswordWith: aString pepper: anotherString numberOfHashIterations: aNumber [
	"This method enables the password protection of command line. All command lines returning true to #requireDeploymentPassword ask a password to be executed. This is usefull in deployment mode of private applications.
	This method accepts a custom pepper for the password hashing. See https://en.wikipedia.org/wiki/Pepper_(cryptography) for more information.
	This method also allows to specify a custom number of hash iterations."

	self current protectCommandLinesByPasswordWith: aString pepper: anotherString numberOfHashIterations: aNumber
]

{ #category : #public }
CommandLinePasswordManager class >> removePasswordProtection [
	self current removePasswordProtection
]

{ #category : #testing }
CommandLinePasswordManager >> hasPasswordSet [
	^ self passwordHash isNotNil
]

{ #category : #private }
CommandLinePasswordManager >> hashString: password [
	| hash |
	hash := self hashingPepper , password.
	(self numberOfHashIterations max: 1) timesRepeat: [ hash := (SHA256 hashMessage: hash) hex ].
	^ hash
]

{ #category : #accessing }
CommandLinePasswordManager >> hashingPepper [
	"If password protection is enabled, developer should change the pepper used for password  cyphering. (https://en.wikipedia.org/wiki/Pepper_(cryptography))"

	^ hashingPepper ifNil: [ hashingPepper := self class defaultHashingPepper ]
]

{ #category : #accessing }
CommandLinePasswordManager >> hashingPepper: aString [
	self hasPasswordSet ifTrue: [ self error: 'The pepper should not be changed when the password is already saved.' ].
	hashingPepper := aString
]

{ #category : #activation }
CommandLinePasswordManager >> isMatchingPassword: aPassword [
	^ (self hashString: aPassword) = self passwordHash
]

{ #category : #accessing }
CommandLinePasswordManager >> numberOfHashIterations [
	"When the password protection is activated, define the number of times the password is hashed."
	
	^ numberOfHashIterations ifNil: [ numberOfHashIterations := self class defaultNumberOfHashIterations ]
]

{ #category : #accessing }
CommandLinePasswordManager >> numberOfHashIterations: anObject [
	self hasPasswordSet ifTrue: [ self error: 'The number of iterations should not be changed when the password is already saved.' ].
	numberOfHashIterations := anObject
]

{ #category : #public }
CommandLinePasswordManager >> password: aString [
	self passwordHash: (self hashString: aString)
]

{ #category : #accessing }
CommandLinePasswordManager >> passwordHash [
	"When this variable is not nil, the command lines are protected by a password. The command line needs to begin with the argument '--password=ThePasswordDefinedByTheDev'"
	
	^ passwordHash
]

{ #category : #accessing }
CommandLinePasswordManager >> passwordHash: aString [	
	passwordHash := aString
]

{ #category : #public }
CommandLinePasswordManager >> protectCommandLinesByPasswordWith: aString pepper: anotherString numberOfHashIterations: aNumber [
	"This method enables the password protection of command line. All command lines returning true to #requireDeploymentPassword ask a password to be executed. This is usefull in deployment mode of private applications.
	This method accepts a custom pepper for the password hashing. See https://en.wikipedia.org/wiki/Pepper_(cryptography) for more information.
	This method also allows to specify a custom number of hash iterations."
	
	"Before setting the pepper and the number of iterations we set the password hash to nil else it migh raise an error since we cannot change pepper/iterations while a password it set."
	self passwordHash: nil.
	self hashingPepper: anotherString.
	self numberOfHashIterations: aNumber.
	self password: aString
]

{ #category : #removing }
CommandLinePasswordManager >> removePasswordProtection [
	self passwordHash: nil
]
