"
I am a project reader that knows how to return a project object from a commit.
New readers can be created by inheriting this class, and will be detected by inheritance.
Then, a reader must implement:

 - #canReadProjectFromCommitish: returns a boolean that indicates if the reader knows how to read the project
 - #readProjectFromCommitish: returns a project object
"
Class {
	#name : #IceProjectReader,
	#superclass : #Object,
	#category : #'Iceberg-Project'
}

{ #category : #'instance creation' }
IceProjectReader class >> readProjectFrom: anIceCommitish [

	| readers |
	readers := (self allSubclasses
		collect: [ :each | each new ])
		sorted: [ :a :b | a priority < b priority ].
	
	^ (readers
		detect: [ :any | any canReadProjectFromCommitish: anIceCommitish ])
			readProjectFromCommitish: anIceCommitish
]

{ #category : #testing }
IceProjectReader >> canReadProjectFromCommitish: aCommitish [
	"Return a boolean indicating if I know how to read the project from the commitish"
	self subclassResponsibility
]

{ #category : #accessing }
IceProjectReader >> priority [

	"By default all readers are important..."
	^ 1
]

{ #category : #reading }
IceProjectReader >> readProjectFromCommitish: aCommitish [
	"Return a project object read from the commitish"
	self subclassResponsibility
]
