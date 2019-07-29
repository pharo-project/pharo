"
I keep global IDs that are used for tracking user activity, e.g. computer id. By default, you should access me by calling #uniqueInstance.

Responsibility:
The IDs that are important to store onto disk (and shared by all images) should be placed in persistedInformation instance variable. On other hand, if you do not want to store it onto disk, create a new instance variable. I can #loadPreferences and #savePreferences onto a disk.

I know computer ID and secret ID. Computer ID is a global UUID that is share among all the images. It is stored on a local disk. Secret ID is use for encrypting information, e.g., class names, method names. You can use #hashForText: method.

Collaborators: I do not collaborate with other classes. I only offer the basic IDs for other frameworks.

Public API and Key Messages

- computerUUID 
- ensureComputerUUID
- hashForText:
- loadPreferences 
- savePreferences.

Before using #computerUUID, you should call #ensureComputerUUID. It will update UUID from the disk or stores existing one if it is not stored yet. I behave like this as automatic storing data to disk on image start-up leads to errors.

Internal Representation and Key Implementation Points.

    Instance Variables
	persistedInformation:		<Dictionary>
	preferences:		<FileReference>

"
Class {
	#name : #GlobalIdentifier,
	#superclass : #Object,
	#instVars : [
		'persistenceStrategy',
		'persistedInformation'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'System-Identification'
}

{ #category : #cleanup }
GlobalIdentifier class >> cleanUp [
	"Clean unique instance."	
	self reset.
]

{ #category : #'class initialization' }
GlobalIdentifier class >> initialize [

	SessionManager default
		register: (ClassSessionHandler forClassNamed: self name)
		inCategory: SessionManager default systemCategory  
		atPriority: SessionManager default defaultPriority - 80.
]

{ #category : #initialization }
GlobalIdentifier class >> initializeUniqueInstance [
	uniqueInstance := self basicNew initialize.
	uniqueInstance ensure.
]

{ #category : #'instance creation' }
GlobalIdentifier class >> new [
	"Users should not create instances of this class.
	Within the framework more instances are created when loading data and when testing."
	self shouldNotImplement
]

{ #category : #initialization }
GlobalIdentifier class >> reset [
	"self reset"
	uniqueInstance := nil
]

{ #category : #'system startup' }
GlobalIdentifier class >> startUp: resuming [

	self reset
]

{ #category : #accessing }
GlobalIdentifier class >> uniqueInstance [
	"self uniqueInstance"
	uniqueInstance ifNil: [ self initializeUniqueInstance ].
	^ uniqueInstance
]

{ #category : #accessing }
GlobalIdentifier >> computerUUID [
	"This value identifies a user logged into the computer.
	Before calling #computerUUID method, you should call #ensure."
	^ persistedInformation at: #computerUUID
]

{ #category : #'load and saving' }
GlobalIdentifier >> ensure [
	self persistenceStrategy ensure: self persistedInformation
]

{ #category : #initialization }
GlobalIdentifier >> initialize [
	super initialize.
	"Dictionary contains all the values that we want to persist on disk."
	persistedInformation := Dictionary new.
	persistedInformation at: #computerUUID put: UUID new asString.
	persistedInformation at: #secretUUID put: UUID new asString.
	persistenceStrategy := GlobalIdentifierPersistence ston.
	persistenceStrategy previous: GlobalIdentifierPersistence fuel.
]

{ #category : #actions }
GlobalIdentifier >> obfuscate: anObject [
	"If you have an information that you want to obfuscate before sending,
	this is likely a good solution for you."
	^ anObject hash bitXor: self secretUUID hash
]

{ #category : #accessing }
GlobalIdentifier >> persistedInformation [
	^ persistedInformation
]

{ #category : #accessing }
GlobalIdentifier >> persistenceStrategy [
	^ persistenceStrategy
]

{ #category : #accessing }
GlobalIdentifier >> persistenceStrategy: anObject [
	persistenceStrategy := anObject
]

{ #category : #accessing }
GlobalIdentifier >> secretUUID [
	"This value can be used for obfuscating an information before sending it.
	See #obfuscate: method.
	Before calling #secretUUID method, you should call #ensure."
	^ persistedInformation at: #secretUUID
]
