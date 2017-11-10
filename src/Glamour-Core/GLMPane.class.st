"
A GLMPane represents the ""physical"" building block of a browser. A pane is presented using a composite presentation (held in the presentations instance var).

It announces:
- GLMMatchingPresentationsChanged
- GLMPresentationsChanged

Instance Variables
	browser:		Browser
	lastActivePresentation:		Presentation
	name:		Symbol
	ports:		Collection of Ports
	presentations:		CompositePresentation
"
Class {
	#name : #GLMPane,
	#superclass : #GLMLoggedObject,
	#instVars : [
		'browser',
		'name',
		'presentations',
		'ports',
		'lastActivePresentation',
		'allowedToNotePresentationsChanged',
		'cachedMatchingPresentations'
	],
	#category : #Glamour-Core
}

{ #category : #'instance creation' }
GLMPane class >> in: aBrowser [ 
	
	^self new initializeIn: aBrowser
]

{ #category : #'instance creation' }
GLMPane class >> named: aString [ 
	^self new initializeNamed: aString
]

{ #category : #'instance creation' }
GLMPane class >> named: aString in: aBrowser [

	^self new initializeNamed: aString in: aBrowser 

]

{ #category : #private }
GLMPane class >> portClass [ 
	 
	^GLMPanePort
]

{ #category : #accessing }
GLMPane >> addPresentation: aPresentation [
	self addPresentations: (OrderedCollection with: aPresentation)
]

{ #category : #accessing }
GLMPane >> addPresentationSilently: each [
	^ presentations
		add:
			(each
				pane: self;
				yourself)
]

{ #category : #accessing }
GLMPane >> addPresentations: aCollection [
	self notingPresentationChangeDo: [ 
		aCollection do: [ :each | self addPresentationSilently: each ] ]
]

{ #category : #converting }
GLMPane >> asGlamorousPane [ 
	 
	^self
]

{ #category : #accessing }
GLMPane >> browser [ 
	 
	^browser
]

{ #category : #accessing }
GLMPane >> browser: aBrowser [
	browser := aBrowser
]

{ #category : #accessing }
GLMPane >> cachedMatchingPresentations [
	^ cachedMatchingPresentations ifNil: [ cachedMatchingPresentations := self matchingPresentations ]
]

{ #category : #accessing }
GLMPane >> cachedMatchingPresentations: anObject [
	^ cachedMatchingPresentations := anObject
]

{ #category : #accessing }
GLMPane >> clearIn: aContext [ 
	self notingPresentationChangeDo: [
		self presentations clear.
		self ports do: [:each | each value: nil in: aContext copy ]]
]

{ #category : #initialization }
GLMPane >> defaultName [

	^'undefined'
]

{ #category : #testing }
GLMPane >> hasBrowser [ 
	 
	^self browser isNil not
]

{ #category : #initialization }
GLMPane >> initialize [
	super initialize.
	ports := OrderedCollection new.
	presentations := GLMCompositePresentation new pane: self; yourself
]

{ #category : #initialization }
GLMPane >> initializeIn: aBrowser [ 
	
	self initializeNamed: self defaultName in: aBrowser
]

{ #category : #initialization }
GLMPane >> initializeNamed: aString [ 
	self initializeNamed: aString in: GLMNoBrowser new  
]

{ #category : #initialization }
GLMPane >> initializeNamed: aString in: aBrowser [ 
	self initialize.
	name := aString ifNil: [ self defaultName ].
	browser := aBrowser
]

{ #category : #private }
GLMPane >> isAllowedToNotePresentationsChanged [
	^ allowedToNotePresentationsChanged ifNil: [allowedToNotePresentationsChanged := true ]
]

{ #category : #testing }
GLMPane >> isLastActivePresentation: aPresentation [
	^ self lastActivePresentation notNil and: [ 
		aPresentation title = self lastActivePresentation title ]
]

{ #category : #accessing }
GLMPane >> lastActivePresentation [
	^ lastActivePresentation
]

{ #category : #accessing }
GLMPane >> lastActivePresentation: aPresentation [
	(self port: #activePresentation) value: aPresentation.
	lastActivePresentation := aPresentation.
	self browser lastActivePresentation: aPresentation
]

{ #category : #accessing }
GLMPane >> matchingPresentations [
	^ cachedMatchingPresentations := presentations matchingPresentations
]

{ #category : #accessing }
GLMPane >> name [ 
	 
	^name
]

{ #category : #accessing }
GLMPane >> name: aString [
	name := aString
]

{ #category : #private }
GLMPane >> notingPresentationChangeDo: aBlock [
	"Executes aBlock. If during the execution of the block one of my presentations change, I will emit announces. I also take care of not entering an infinite loop."
	| oldPresentations oldMatchingPresentations | 
	self isAllowedToNotePresentationsChanged ifFalse: [
		aBlock value.
		^ self].
	"this should create a context and then check for it before announcing.
	Like this we can deal with the problem of announcing the same announcement twice"
	allowedToNotePresentationsChanged := false.
	
	oldPresentations := presentations presentations copy. 
	oldMatchingPresentations := self cachedMatchingPresentations.
	aBlock value. 
	presentations presentations  ~= oldPresentations ifTrue: [
		self announce: ((GLMPresentationsChanged new) 
						pane: self; 
						oldPresentations: oldPresentations; 
						yourself)]. 
	self matchingPresentations ~= oldMatchingPresentations ifTrue: [
		self announce: ((GLMMatchingPresentationsChanged new) 
						pane: self; 
						oldMatchingPresentations: oldMatchingPresentations; 
						yourself)].
	allowedToNotePresentationsChanged := true.

]

{ #category : #accessing }
GLMPane >> port: aSymbol [
	"returns the port with the given name"
	^ ports detect: [ :each | each name = aSymbol ] ifNone: [
		ports addLast: (
			(self class portClass new) 
						pane: self; 
						name: aSymbol; 
						yourself) ]
]

{ #category : #private }
GLMPane >> portEvent: aPortEvent [

	"we unregister all presentations from announcements because they will get replaced anyway"
	aPortEvent portName = #entity ifTrue: [ self unregisterFromAllAnnouncements ].
	self browser innerPortEvent: aPortEvent.
	self presentations outerPortEvent: aPortEvent
]

{ #category : #accessing }
GLMPane >> ports [
	^ ports
]

{ #category : #copying }
GLMPane >> postCopy [
	| newPort |
	super postCopy.
	self flag: 'self unregisterFromAllAnnouncements ?'.
	presentations := presentations copy.
	ports := ports collect: [:each | 
		newPort := each copy.
		newPort pane: self.
		self flag: 'what about value?'.
		newPort ]
]

{ #category : #accessing }
GLMPane >> presentations [
	^ presentations
]

{ #category : #accessing }
GLMPane >> presentations: aCompositePresentation [
	self notingPresentationChangeDo: [
		presentations pane: nil.
		presentations resetAnnouncer.
		aCompositePresentation pane: self.
		presentations := aCompositePresentation.
		presentations initializePresentation ]
]

{ #category : #printing }
GLMPane >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self identityHash printString;
		space;
		nextPutAll: self name;
		nextPut: $)
]

{ #category : #rendering }
GLMPane >> renderGlamorouslyOn: aRenderer [ 
	 
	^aRenderer renderPane: self
]

{ #category : #announcements }
GLMPane >> resetAnnouncer [
	super resetAnnouncer.
	self presentations do: [ :each | each resetAnnouncer ]
]

{ #category : #updating }
GLMPane >> unregisterFromAllAnnouncements [
	self presentations unregisterFromAllAnnouncements
]

{ #category : #updating }
GLMPane >> update [ 
	self notingPresentationChangeDo: [
		"nothing in particular. 
		just make sure that matching presentations are recomputed"].
	self presentations do: [ :each | each update ]
]

{ #category : #updating }
GLMPane >> updateToolbar [

	self announce: (GLMPresentationToolbarUpdated new presentation: self presentations)
]

{ #category : #enumerating }
GLMPane >> withAllPanes [
	| result |
	result := OrderedCollection new.
	self withAllPanesAndPresentationsDo: [:each |
		(each isKindOf: GLMPane) ifTrue: [ result add: each ]].
	^ result
]

{ #category : #enumerating }
GLMPane >> withAllPanesAndPresentationsDo: aBlock [
	aBlock value: self.
	self presentations withAllPanesAndPresentationsDo: aBlock
]

{ #category : #enumerating }
GLMPane >> withAllPresentations [
	| result |
	result := OrderedCollection new.
	self withAllPanesAndPresentationsDo: [:each |
		(each isKindOf: GLMPresentation) ifTrue: [ result add: each ]].
	^ result
]
