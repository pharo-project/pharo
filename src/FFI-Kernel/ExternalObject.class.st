"
External objects represent entities that are not part of the Smalltalk universe. They are accessed using a unique handle which is interpreted depending on the actual entity that is represented. 

Instance variables:
	handle	<ByteArray | ExternalAddress>
"
Class {
	#name : #ExternalObject,
	#superclass : #Object,
	#instVars : [
		'handle'
	],
	#category : #'FFI-Kernel'
}

{ #category : #'class initialization' }
ExternalObject class >> initialize [
	"ExternalObject initialize"
	(Smalltalk classNamed: #SessionManager)
		ifNotNil: [:sessionManagerClass|
			sessionManagerClass default
				registerSystemClassNamed: self name
				atPriority: 60]
		ifNil: [Smalltalk addToStartUpList: self after: (Smalltalk classNamed: #ShortRunArray)].	
]

{ #category : #'system startup' }
ExternalObject class >> install [
	"Notify all instances of the receiver that we're coming up on a new platform.
	Note: The default implementation does nothing since the general external
	objects are cleaned up by ExternalAddress>>startUp: but subclasses may
	implement this method so that the appropriate action for existing instances can
	be taken."
]

{ #category : #'system startup' }
ExternalObject class >> installSubclasses [
	"Notify all the subclasses of ExternalObject that we are starting up on a new platform."
	self withAllSubclassesDo:[:cls| cls install].
]

{ #category : #'system startup' }
ExternalObject class >> startUp: resuming [
	"The system is coming up. If it is on a new platform, clear out the existing handles."
	ExternalAddress startUp: resuming. "Make sure handles are invalid"
	resuming ifTrue:[self installSubclasses].

]

{ #category : #private }
ExternalObject >> getHandle [
	"Private. Return the handle used to represent the external entitiy."
	^handle
]

{ #category : #testing }
ExternalObject >> isExternalAddress [
	"Return true if the receiver describes the address of an object in the outside world"
	^false
]

{ #category : #testing }
ExternalObject >> isNull [
	"Answer true if the receiver currently is a NULL pointer"
	^handle == nil or:[handle isNull]
]

{ #category : #private }
ExternalObject >> setHandle: anObject [
	"Private. Set the handle used to represent the external entity."
	handle := anObject
]
