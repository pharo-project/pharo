"
I announce that a MCVersion was saved into a MCRepository.
"
Class {
	#name : #MCVersionSaved,
	#superclass : #Announcement,
	#instVars : [
		'version',
		'repository'
	],
	#category : #'Monticello-Announcements'
}

{ #category : #'instance creation' }
MCVersionSaved class >> version: aMCVersion repository: aMCRepository [

	^ self basicNew
		initializeWithVersion: aMCVersion
		repository: aMCRepository;
		yourself
]

{ #category : #comparing }
MCVersionSaved >> = another [

	^ (self class = another class
		and: [ self version = another version ])
		and: [ self repository = another repository ]
]

{ #category : #comparing }
MCVersionSaved >> hash [

	^ (self class hash
		bitXor: self version hash)
		bitXor: self repository hash
]

{ #category : #initialization }
MCVersionSaved >> initializeWithVersion: aMCVersion repository: aMCRepository [

	self initialize.
	
	version := aMCVersion.
	repository := aMCRepository.
]

{ #category : #printing }
MCVersionSaved >> printOn: aStream [

	super printOn: aStream.
	
	aStream 
		nextPut: $(;
		nextPutAll: version info name;
		nextPut: $@;
		nextPutAll: repository description;
		nextPut: $).
]

{ #category : #accessing }
MCVersionSaved >> repository [

	^ repository
]

{ #category : #accessing }
MCVersionSaved >> version [

	^ version
]
