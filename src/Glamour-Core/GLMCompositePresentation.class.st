"
A composite presentation offers means to control the composition of multiple presentations by providing the arrangement of these presentations.
"
Class {
	#name : #GLMCompositePresentation,
	#superclass : #GLMPresentation,
	#instVars : [
		'presentations',
		'arrangement'
	],
	#category : #'Glamour-Core'
}

{ #category : #examples }
GLMCompositePresentation class >> gtExampleCompositePresentation [
	<gtExample>

	^ self new
		list;
		table;
		tree
]

{ #category : #openning }
GLMCompositePresentation class >> openOn: anObject [ 
	^ self new openOn: anObject
]

{ #category : #scripting }
GLMCompositePresentation >> accordionArrangement [
	^ self arrangement: GLMAccordionArrangement new
]

{ #category : #scripting }
GLMCompositePresentation >> actionList [
	^ self custom: GLMActionListPresentation new
]

{ #category : #accessing }
GLMCompositePresentation >> add: aPresentation [
	self pane notingPresentationChangeDo: [
		self presentations add: aPresentation.
		aPresentation pane: self pane ].
	^ aPresentation
]

{ #category : #accessing }
GLMCompositePresentation >> allSatisfy: aBlock [
	^ self presentations allSatisfy: aBlock 
]

{ #category : #accessing }
GLMCompositePresentation >> anySatisfy: aBlock [
	^ self presentations anySatisfy: aBlock 
]

{ #category : #accessing }
GLMCompositePresentation >> arrangement [
	^ arrangement ifNil: [arrangement := GLMTabbedArrangement of: self]
]

{ #category : #accessing }
GLMCompositePresentation >> arrangement: anArrangement [
	anArrangement composite: self.
	^ arrangement := anArrangement
]

{ #category : #accessing }
GLMCompositePresentation >> at: aNumber [
	^ self presentations at: aNumber 
]

{ #category : #accessing }
GLMCompositePresentation >> clear [
	arrangement := nil.
	presentations := nil
]

{ #category : #accessing }
GLMCompositePresentation >> collect: aBlock [
	^ self copy presentations: (self presentations collect: aBlock ); yourself
]

{ #category : #building }
GLMCompositePresentation >> compose [
	"by default this method is empty.
	override this method in subclasses to define a custom browser"
]

{ #category : #scripting }
GLMCompositePresentation >> composite: aBlock [
	| composite |
	composite := GLMCompositePresentation new.
	aBlock value: composite.
	^ self custom: composite
]

{ #category : #scripting }
GLMCompositePresentation >> custom: aPresentation [
	^ self add: aPresentation "sourceContext: thisContext sender"
]

{ #category : #'scripting opening' }
GLMCompositePresentation >> defaultRenderer [ 
	 
	^GLMRenderer subclasses first new
]

{ #category : #scripting }
GLMCompositePresentation >> diff [
	^ self custom: GLMDiffPresentation new
]

{ #category : #accessing }
GLMCompositePresentation >> do: aBlock [
	^ self presentations do: aBlock 
]

{ #category : #scripting }
GLMCompositePresentation >> dropDownList [
	^ self custom: GLMDropDownListPresentation new
]

{ #category : #scripting }
GLMCompositePresentation >> dynamic [
	^ self custom: GLMDynamicPresentation new
]

{ #category : #scripting }
GLMCompositePresentation >> finder [
	^ self custom: GLMFinder new
]

{ #category : #accessing }
GLMCompositePresentation >> first [
	^ self presentations first
]

{ #category : #accessing }
GLMCompositePresentation >> includes: aPresentation [
	^ self presentations includes: aPresentation 
]

{ #category : #initialization }
GLMCompositePresentation >> initialize [ 
	super initialize.
	self compose
]

{ #category : #initialization }
GLMCompositePresentation >> initializePresentation [

	super initializePresentation.
	self do: [:each| each initializePresentation]
]

{ #category : #accessing }
GLMCompositePresentation >> isEmpty [
	^ self presentations isEmpty
]

{ #category : #scripting }
GLMCompositePresentation >> label [
	^ self custom: GLMLabelPresentation new
]

{ #category : #accessing }
GLMCompositePresentation >> last [
	^ self presentations last
]

{ #category : #scripting }
GLMCompositePresentation >> list [ 
	^ self custom: GLMListPresentation new
]

{ #category : #accessing }
GLMCompositePresentation >> matchingPresentations [
	^ self presentations select: [:each | each matches]
]

{ #category : #scripting }
GLMCompositePresentation >> morph [
	^ self custom: GLMMorphPresentation new
]

{ #category : #'scripting opening' }
GLMCompositePresentation >> open [ 
	 
	^self openWith: self defaultRenderer
]

{ #category : #OSWindow }
GLMCompositePresentation >> openInExternalWindow [
    ^ self defaultRenderer openInExternalWindow: self
]

{ #category : #OSWindow }
GLMCompositePresentation >> openInExternalWindowOn: anObject [ 
    ^ self openOn: anObject inExternalWindowWith: self defaultRenderer
]

{ #category : #OSWindow }
GLMCompositePresentation >> openInExternalWindowWith: aRenderer [ 
    ^ aRenderer openInExternalWindow: self
]

{ #category : #'scripting opening' }
GLMCompositePresentation >> openOn: anObject [ 
	 
	^self 
		openOn: anObject 
		with: self defaultRenderer
]

{ #category : #OSWindow }
GLMCompositePresentation >> openOn: anObject inExternalWindowWith: aRenderer [ 
     
    self startOn: anObject. 
    ^ aRenderer openInExternalWindow: self
]

{ #category : #'scripting opening' }
GLMCompositePresentation >> openOn: anObject with: aRenderer [ 
	 
	self startOn: anObject. 
	^ self openWith: aRenderer
]

{ #category : #'scripting opening' }
GLMCompositePresentation >> openWith: aRenderer [ 
	 
	^ aRenderer open: self
]

{ #category : #events }
GLMCompositePresentation >> outerPortEvent: aPortEvent [
	super outerPortEvent: aPortEvent.
	self do: [:each| each outerPortEvent: aPortEvent]
]

{ #category : #accessing }
GLMCompositePresentation >> pane [
	^ pane ifNil: [
		pane := (GLMPane named: 'root' in: GLMNoBrowser new) 
				presentations: self; 
				yourself]
]

{ #category : #accessing }
GLMCompositePresentation >> pane: aPane [ 
	super pane: aPane.
	self presentations do: [:each | each pane: aPane ]
]

{ #category : #copying }
GLMCompositePresentation >> postCopy [
	super postCopy.
	presentations ifNotNil: [ :arg | 
		presentations := presentations collect: [ :each | each copy ] ].
	arrangement ifNotNil: [ :arg | 
		arrangement := arrangement copy.
		arrangement composite: self ]
]

{ #category : #accessing }
GLMCompositePresentation >> presentations [
	^ presentations ifNil: [presentations := OrderedCollection new]
]

{ #category : #accessing }
GLMCompositePresentation >> presentations: anObject [
	presentations := anObject
]

{ #category : #rendering }
GLMCompositePresentation >> renderGlamorouslyOn: aRenderer [
	"we delegate to the arrangement to dispatch the type of the actual arrangement of the nested presentations"
	self registerAnnouncements.
	^ self arrangement renderGlamorouslyOn: aRenderer
]

{ #category : #announcements }
GLMCompositePresentation >> resetAnnouncer [
	super resetAnnouncer.
	self presentations do: [:each | each resetAnnouncer ]
]

{ #category : #accessing }
GLMCompositePresentation >> second [
	^ self presentations second
]

{ #category : #accessing }
GLMCompositePresentation >> select: aBlock [
	^ self presentations select: aBlock
]

{ #category : #accessing }
GLMCompositePresentation >> size [
	^ self presentations size
]

{ #category : #scripting }
GLMCompositePresentation >> smalltalkCode [
	self deprecated: 'Use #pharoMethod (if you need to display a method) 
	or #pharoScript (if you need to display snippets of code that do not belong to a method)'.
	^ self custom: GLMSmalltalkCodePresentation new
]

{ #category : #scripting }
GLMCompositePresentation >> stackedArrangement [
	^ self stackedVerticallyArrangement
]

{ #category : #scripting }
GLMCompositePresentation >> stackedVerticallyArrangement [
	^ self arrangement: (GLMStackedVerticallyArrangement of: self)
]

{ #category : #'scripting opening' }
GLMCompositePresentation >> startOn: anObject [ 
	
	self entity: anObject
]

{ #category : #scripting }
GLMCompositePresentation >> tabbedArrangement [
	^ self arrangement: GLMTabbedArrangement new
]

{ #category : #scripting }
GLMCompositePresentation >> table [
	^ self custom: GLMTablePresentation new
]

{ #category : #scripting }
GLMCompositePresentation >> tabulator [
	^ self custom: GLMTabulator new
]

{ #category : #scripting }
GLMCompositePresentation >> text [ 
	^ self custom: GLMRubricTextPresentation new
]

{ #category : #scripting }
GLMCompositePresentation >> tree [ 
	^ self custom: GLMTreePresentation new
]

{ #category : #announcements }
GLMCompositePresentation >> unregisterFromAllAnnouncements [
	super unregisterFromAllAnnouncements.
	self presentations do: [:each | each unregisterFromAllAnnouncements ]
]

{ #category : #updating }
GLMCompositePresentation >> update [
	self pane ports do: [:each |
		(self validate: each value on: each) ifFalse: [each resetValue] ].
	self presentations do: [ :each | each update ].
	self announce: (GLMPresentationUpdated new presentation: self).
]

{ #category : #scripting }
GLMCompositePresentation >> verticallyStackedArrangement [
	^ self stackedVerticallyArrangement
]

{ #category : #enumerating }
GLMCompositePresentation >> withAllPanesAndPresentationsDo: aBlock [
	super withAllPanesAndPresentationsDo: aBlock.
	self do: [:each | each withAllPanesAndPresentationsDo: aBlock ]
]
