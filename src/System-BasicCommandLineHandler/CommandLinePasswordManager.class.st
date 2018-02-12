"
I provide a management of the password that is needed to execute the commands throught the command line handler. Use public API to define a new password a remove the current one.

The password is not save in clear text but salted
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

{ #category : #private }
CommandLinePasswordManager >> hashString: password [
	| hash |
	hash := self hashingPepper , password.
	(self numberOfHashIterations min: 1) timesRepeat: [ hash := (SHA256 hashMessage: hash) hex ].
	^ hash
]

{ #category : #accessing }
CommandLinePasswordManager >> hashingPepper [
	"If password protection is enabled, developer should change the pepper used for password  cyphering. (https://en.wikipedia.org/wiki/Pepper_(cryptography))"

	^ hashingPepper ifNil: [ hashingPepper := self class defaultHashingPepper ]
]

{ #category : #accessing }
CommandLinePasswordManager >> hashingPepper: aString [
	hashingPepper := aString
]

{ #category : #activation }
CommandLinePasswordManager >> isGoodPassword: aPassword [
	^ (self hashString: aPassword) = self class passwordHash
]

{ #category : #accessing }
CommandLinePasswordManager >> numberOfHashIterations [
	"When the password protection is activated, define the number of times the password is hashed."
	
	^ numberOfHashIterations ifNil: [ numberOfHashIterations := self class defaultNumberOfHashIterations ]
]

{ #category : #accessing }
CommandLinePasswordManager >> numberOfHashIterations: anObject [
	numberOfHashIterations := anObject
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

	self hashingPepper: anotherString.
	self numberOfHashIterations: aNumber.
	passwordHash := self hashString: aString
]

{ #category : #removing }
CommandLinePasswordManager >> removePasswordProtection [
	self passwordHash: nil
]

{ #category : #testing }
CommandLinePasswordManager >> requirePassword [
	^ self passwordHash isNotNil
]
