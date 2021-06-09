"
I am an abstract group of document items.
"
Class {
	#name : #PRDocumentGroup,
	#superclass : #PRDocumentItem,
	#instVars : [
		'children'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #testing }
PRDocumentGroup class >> isAbstract [ 

	^ false
]

{ #category : #'instance creation' }
PRDocumentGroup class >> with: aDocumentItem [
	^ self new
		add: aDocumentItem;
		yourself
]

{ #category : #'instance creation' }
PRDocumentGroup class >> withAll: aCollection [
	^ self new
		addAll: aCollection;
		yourself
]

{ #category : #comparing }
PRDocumentGroup >> = anObject [
	^ super = anObject
		and: [ self children = anObject children ]
]

{ #category : #visiting }
PRDocumentGroup >> accept: aVisitor [
	<ignoreForCoverage "This method is ignored for test coverage because it is overriden in all subclasses and these subclasses don't need to do a super-send.">
	^ aVisitor visitDocumentGroup: self
]

{ #category : #adding }
PRDocumentGroup >> add: aDocumentItem [
	aDocumentItem parent: self. 
	self setChildren: (self children copyWith: aDocumentItem)
]

{ #category : #adding }
PRDocumentGroup >> addAll: aCollection [
	self setChildren: self children , aCollection
]

{ #category : #accessing }
PRDocumentGroup >> children [
	"Answer the children of the receiver."

	^ children
]

{ #category : #testing }
PRDocumentGroup >> hasChildren [
	^ self children isNotEmpty
]

{ #category : #comparing }
PRDocumentGroup >> hash [
	^ super hash bitXor: self children hash
]

{ #category : #initialization }
PRDocumentGroup >> initialize [
	super initialize.
	self setChildren: #()
]

{ #category : #testing }
PRDocumentGroup >> isGroup [
	^true
]

{ #category : #copying }
PRDocumentGroup >> postCopy [
	super postCopy.
	self setChildren: (self children
		collect: [ :each | each copy ])
]

{ #category : #initialization }
PRDocumentGroup >> setChildren: aCollection [
	children := aCollection
]

{ #category : #'accessing - dynamic' }
PRDocumentGroup >> size [
	^ self children size
]

{ #category : #'accessing - dynamic' }
PRDocumentGroup >> text [
	^ String streamContents: [ :stream |
		self children do: [ :each |
			stream nextPutAll: each text ] ]
]
