"
Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, ""I represent a paragraph of text"".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them. 

Public API and Key Messages

- message one   
- message two 
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	builder:		<Object>
	changes:		<Object>
	children:		<Object>
	entryName:		<Object>
	originalTree:		<LGitTree> Contains the original contents on which this builder bases. Warning: this will not reflect the actual contents of the builder. Use #buildTree instead.
	parent:		<Object>


    Implementation Points
"
Class {
	#name : #IceLibgitTreeBuilder,
	#superclass : #Object,
	#instVars : [
		'parent',
		'entryName',
		'builder',
		'children',
		'changes',
		'initialTree',
		'hasChanges'
	],
	#category : #'Iceberg-Libgit-Core'
}

{ #category : #'instance creation' }
IceLibgitTreeBuilder class >> on: aLGitTree [
	"Creates a tree builder on the root of the repository"
	^ self new initializeWithTree: aLGitTree
]

{ #category : #modifying }
IceLibgitTreeBuilder >> addEntryNamed: name withContents: aString [
	self builder 
		addEntry: (LGitBlob of: self repository fromString: aString)
		filename: name.
		
	self changes add: name.
	hasChanges := true
]

{ #category : #building }
IceLibgitTreeBuilder >> buildTree [
	self children keysAndValuesDo: [ :childEntryName :childBuilder |
		childBuilder hasChanges ifTrue: [ 
			self builder 
				addEntry: childBuilder buildTree
				filename: childEntryName
		]
	].

	^ self builder newTree
]

{ #category : #accessing }
IceLibgitTreeBuilder >> builder [
	^ builder ifNil: [ 
		builder := LGitTreeBuilder 
			of: self repository
			fromTree: self initialTree ]

]

{ #category : #accessing }
IceLibgitTreeBuilder >> changes [
	^ changes ifNil: [ changes := OrderedCollection new ]
]

{ #category : #accessing }
IceLibgitTreeBuilder >> children [
	^ children ifNil: [ children := Dictionary new ]
]

{ #category : #modifying }
IceLibgitTreeBuilder >> deleteAll [
	self builder clear.

]

{ #category : #patches }
IceLibgitTreeBuilder >> directory [
	^ self 
]

{ #category : #querying }
IceLibgitTreeBuilder >> entryByName: aString [
	^ self children 
		at: aString 
		ifAbsentPut: [  
			self class new 
				initializeWithParent: self 
				entryName: aString ]
]

{ #category : #querying }
IceLibgitTreeBuilder >> entryByPath: aString [
	^ (aString = '.' )
		ifTrue: [ self ]
		ifFalse: [( ($/ split: aString) reject: #isEmpty)
			inject: self 
			into: [ :treeBuilder :name | treeBuilder entryByName: name ]]
]

{ #category : #accessing }
IceLibgitTreeBuilder >> entryName [
	^ entryName
]

{ #category : #querying }
IceLibgitTreeBuilder >> hasChanges [
	^ hasChanges
		or: [ initialTree isNil "It means that this is a new tree"
		or: [ self children anySatisfy: #hasChanges ]]

]

{ #category : #accessing }
IceLibgitTreeBuilder >> initialTree [
	^ initialTree

]

{ #category : #initializing }
IceLibgitTreeBuilder >> initialize [
	super initialize.
	hasChanges := false.
	
]

{ #category : #initializing }
IceLibgitTreeBuilder >> initializeWithParent: anIceTreeBuilder entryName: aString [
	| treeEntry |
	parent := anIceTreeBuilder.
	entryName := aString.

	treeEntry := parent builder getEntryNamed: entryName.
	treeEntry isReady "i.e. the entry alredy exists in parent tree"
		ifTrue: [ initialTree := treeEntry tree ]
		ifFalse: [ 
			"The entry didn't exist so this is a new entry"
			hasChanges := true ]

]

{ #category : #initializing }
IceLibgitTreeBuilder >> initializeWithTree: aLGitTree [
	"This is a builder on the root of a repository"
	initialTree := aLGitTree 
]

{ #category : #accessing }
IceLibgitTreeBuilder >> parent [
	^ parent
]

{ #category : #printing }
IceLibgitTreeBuilder >> printOn: aStream [
	aStream 
		nextPutAll: self class name;
		nextPut: $(.
	self printPathOn: aStream.
	aStream nextPut: $)

]

{ #category : #printing }
IceLibgitTreeBuilder >> printPathOn: aStream [
	parent 
		ifNil: [ 
			aStream nextPutAll: initialTree repository workingDirectoryName ]
		ifNotNil: [ 	
			parent printPathOn: aStream.
			aStream nextPut: $/;
				nextPutAll: entryName ] 
]

{ #category : #accessing }
IceLibgitTreeBuilder >> repository [
	^ parent 
		ifNotNil: [ self parent repository ]
		ifNil: [ self initialTree owner ]
]
