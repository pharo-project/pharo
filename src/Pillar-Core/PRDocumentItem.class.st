"
I am an abstract superclass for the document hierarchy. My subclasses include all the basic elements to represent a document.

!! Description

Most of the time I will be used by a visitor that will visit me. Each of my subclasses can have its own visitor.
I'm usually contained inside a PRDocumentGroup at the exception of PRDocument.

!! Public API and Key Messages

- #parser. This class method returns the default parser for this document model.


!! Internal Representation and Key Implementation Points.


-	counter:		I am a counter used by some document items. Maybe I can be improve because not all items need a counter.

"
Class {
	#name : #PRDocumentItem,
	#superclass : #PRObject,
	#instVars : [
		'parent',
		'counter',
		'start',
		'stop'
	],
	#category : #'Pillar-Core-Model'
}

{ #category : #comparing }
PRDocumentItem >> = anObject [
	^ self species = anObject species
]

{ #category : #visiting }
PRDocumentItem >> accept: aVisitor [
	<ignoreForCoverage "This method is ignored for test coverage because it is overriden in all subclasses and these subclasses don't need to do a super-send.">
	^ aVisitor visitDocumentItem: self
]

{ #category : #accessing }
PRDocumentItem >> counter [
	^ counter
]

{ #category : #accessing }
PRDocumentItem >> counter: anObject [
	counter := anObject
]

{ #category : #'gt-inspection' }
PRDocumentItem >> gtInspectPillarModel: composite [
	<gtInspectorPresentationOrder: 50>
	
	composite tree
		shouldValidate: false;
		title: 'Model';
		display: [ :each | each hasChildren ifTrue: [ each children ] ifFalse: [ #() ] ];
		children: [ :each | each hasChildren ifTrue: [ each children ] ifFalse: [ #() ] ];
		format: [ :each | (each class name withoutPrefix: 'PR'), ' - ', (each text contractTo: 50)] 
]

{ #category : #testing }
PRDocumentItem >> hasChildren [
	^ false
]

{ #category : #comparing }
PRDocumentItem >> hash [
	^ self species hash
]

{ #category : #initialization }
PRDocumentItem >> initialize [
	super initialize.
	counter := PRNotFoundCounter maxLevel: 5
]

{ #category : #testing }
PRDocumentItem >> isDocument [
	^self class = PRDocument
]

{ #category : #testing }
PRDocumentItem >> isGroup [
	^false
]

{ #category : #testing }
PRDocumentItem >> isHeader [
	^ false
]

{ #category : #testing }
PRDocumentItem >> isLink [
	^ false
]

{ #category : #accessing }
PRDocumentItem >> parent [

	^ parent
]

{ #category : #accessing }
PRDocumentItem >> parent: aDocumentGroup [

	parent := aDocumentGroup
]

{ #category : #helpers }
PRDocumentItem >> parser [
  ^ self class parser
]

{ #category : #accessing }
PRDocumentItem >> start [
	^ start
]

{ #category : #accessing }
PRDocumentItem >> start: anObject [
	start := anObject
]

{ #category : #accessing }
PRDocumentItem >> stop [
	^ stop
]

{ #category : #accessing }
PRDocumentItem >> stop: anObject [
	stop := anObject
]

{ #category : #'accessing - dynamic' }
PRDocumentItem >> text [
	"Answer a string representation of the receiver."

	^ String new
]
