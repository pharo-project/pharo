"
I am an abstract superclass for objects that fulfill the Guide role in the Guide/Visitor pattern. My subclasses know how to traverse a filesystem in a specific order, ""showing"" the files and directories they encounter to a visitor.

visitor
	An object that fulfills the Visitor role and implements the visitor protocol.
	
work
	An OrderedCollection, used to keep track of filesystem nodes that have not yet been visited
"
Class {
	#name : #FileSystemGuide,
	#superclass : #Object,
	#instVars : [
		'visitor',
		'work',
		'selectChildren'
	],
	#category : #'FileSystem-Core-Base-Guide'
}

{ #category : #'instance creation' }
FileSystemGuide class >> for: aVisitor [
	^ self basicNew initializeWithVisitor: aVisitor
]

{ #category : #'instance creation' }
FileSystemGuide class >> show: aReference to: aVisitor [
	^ (self for: aVisitor) show: aReference
]

{ #category : #'instance creation' }
FileSystemGuide class >> show: aReference to: aVisitor selecting: aBlock [
	^ (self for: aVisitor)
		selectChildren:  aBlock;
		show: aReference
]

{ #category : #initialization }
FileSystemGuide >> initialize [

	super initialize.
	work := OrderedCollection new
]

{ #category : #initialization }
FileSystemGuide >> initializeWithVisitor: aVisitor [
	self initialize.
	visitor := aVisitor.
	
]

{ #category : #removing }
FileSystemGuide >> pop [
	^ work removeLast
]

{ #category : #adding }
FileSystemGuide >> push: anObject [
	work add: anObject
]

{ #category : #showing }
FileSystemGuide >> pushAll: aCollection [ 
	aCollection do: [ :ea | self push: ea ]
]

{ #category : #accessing }
FileSystemGuide >> selectChildren [
	^ selectChildren
]

{ #category : #accessing }
FileSystemGuide >> selectChildren: aBlock [
	"With this block you can control how the guide spreads over directories.
	Example: 
		self selectChildren: [ :parentEntry | parentEntry isSymlink not ].
		This will prevent the outer visitor to see any children of symlinked directories.
		
	Since the guides essentially rearrange the files visited controlling which children
	you see is the main concern of the guide. All the other visiting aspects can be 
	controlled in the visitor."
	selectChildren := aBlock
]

{ #category : #testing }
FileSystemGuide >> shouldVisitChildrenOf: anEntry [
	^ selectChildren 
		ifNil: [ true ]
		ifNotNil: [ selectChildren cull: anEntry ]
]

{ #category : #showing }
FileSystemGuide >> show: aReference [
	self subclassResponsibility
]

{ #category : #accessing }
FileSystemGuide >> top [
	^ work removeFirst
]

{ #category : #showing }
FileSystemGuide >> whileNotDoneDo: aBlock [
	[ work isEmpty ] whileFalse: [ aBlock value ]
]
