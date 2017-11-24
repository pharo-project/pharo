"
A GLMFinder models a browsers that behaves like the Mac Finder: whenever the selection port is set on one pane, a new one is created to the right with the selection as entity. The Finder opens the first pane on the entity.

The Finder communicates with the Renderer 

Input ports:
- entity: this is passed to the first pane

Output ports:
- selection: this port is populated with the value from the last selection port from one of the panes
"
Class {
	#name : #GLMFinder,
	#superclass : #GLMImplicitBrowser,
	#instVars : [
		'hasFixedSizePanes',
		'fixedSizePanes',
		'lastActivePresentation',
		'firstTransmission'
	],
	#category : #'Glamour-Browsers-Other'
}

{ #category : #private }
GLMFinder >> addPaneFromInner: aPort [ 
	"This method adds a new pane at the end of the finder. Data comes from a port that is in a pane inside the current browser."
	| newPane lastActive |
	"the lastActivePresentation is needed for knowing what presentation to show by default"
	lastActive := self panes isEmpty ifTrue: [nil] ifFalse: [ self lastActivePresentation ].
	newPane := GLMPane named: (self panes size + 1) asString in: self.
	newPane lastActivePresentation: lastActive.
	self panes addLast: newPane.
	self newTransmission
		addActiveOrigin: aPort; 
		destination: (newPane port: #entity); 
"		presentations: (self presentations copy); "
		transmit. 


	"This code was supposed to offer custom transmissions"
"	realTransmissions := self transmissions collect: [:oldTransmission |  
			newTransmission := GLMTransmission new ensureReplacePresentationsStrategy. 
			oldTransmission activeOrigins do: [:eachPort | 
				newTransmission addActiveOrigin: (aPort pane port: eachPort name)]. 
			oldTransmission passiveOrigins do: [:eachPort | 
				newTransmission addPassiveOrigin: (aPort pane port: eachPort name)]. 
			newTransmission 
				destination: (newPane port: #entity); 
				addPresentations: (self presentations collect: #copy);
				addPresentations: (oldTransmission presentations collect: #copy)] . 
	[realTransmissions isEmpty] assert.
	(realTransmissions select: [:each | each originatesAt: aPort]) 
		do: [:each | each transmit]. "
]

{ #category : #private }
GLMFinder >> addPaneFromOuter: aPort [ 
	"This method adds a new pane at the end of the finder."
	|newPane |
	newPane := self panes addLast: (GLMPane named: (self panes size + 1) asString in: self).
	self newFirstTransmission
		addActiveOrigin: aPort; 
		destination: (newPane port: #entity); 
		transmit
]

{ #category : #scripting }
GLMFinder >> firstTransmission [
	^ firstTransmission ifNil: [
		firstTransmission := GLMTransmission new ensureReplacePresentationsStrategy ]
]

{ #category : #scripting }
GLMFinder >> fixedSizePanes [
	^ fixedSizePanes ifNil: [fixedSizePanes := 2]
]

{ #category : #scripting }
GLMFinder >> fixedSizePanes: anInteger [
	self hasFixedSizePanes: true.
	fixedSizePanes := anInteger
]

{ #category : #scripting }
GLMFinder >> fromPrevious: aPortSymbol [
	self flag: 'needs revisiting. it does not work anymore'.
	self scriptTransmission addActiveOrigin: (GLMPort new name: aPortSymbol)
]

{ #category : #accessing }
GLMFinder >> hasFixedSizePanes [
	^ hasFixedSizePanes ifNil: [hasFixedSizePanes := true]
]

{ #category : #accessing }
GLMFinder >> hasFixedSizePanes: aBoolean [
	hasFixedSizePanes := aBoolean
]

{ #category : #events }
GLMFinder >> innerPortEvent: aPortEvent [ 
	aPortEvent isEntityPort ifFalse: [
		"We don't want to resend the transmission, but use a new SimpleTransmission." 
		(self pane port: aPortEvent portName) value: aPortEvent value]. 

	aPortEvent isSelectionPort ifTrue: [
		| index toReplace |
		index := self panes reversed indexOf: aPortEvent pane.
		(self panes reversed first: index - 2) do: [ :each |  
			self removeLastPane.
			self announce: (GLMPaneRemoved pane: each fromBrowser: self) ].
		index <= 1 
			ifTrue: [
				self addPaneFromInner: aPortEvent port.
				self announce: 
					(GLMPaneAdded 
						pane: self panes last 
						atPosition: self panes size 
						inBrowser: self) ]
			ifFalse: [
				toReplace := self removeLastPane.
				self lastActivePresentation: toReplace lastActivePresentation.
				self addPaneFromInner: aPortEvent port.
				self announce: 
					(GLMPaneReplaced 
							oldPane: toReplace 
							newPane: self panes last
							fromBrowser: self ) ] 
	]
]

{ #category : #private }
GLMFinder >> lastActivePresentation [
	^ lastActivePresentation ifNil: [ 
		self panes last lastActivePresentation ]
]

{ #category : #private }
GLMFinder >> lastActivePresentation: aPresentation [
	lastActivePresentation := aPresentation
]

{ #category : #scripting }
GLMFinder >> newFirstTransmission [
	^ firstTransmission 
		ifNil: [ self newTransmission ]
		ifNotNil: [ self firstTransmission copy ]
]

{ #category : #events }
GLMFinder >> outerPortEvent: aPortEvent [ 
	 
	| toReplace |
	super outerPortEvent: aPortEvent.
	aPortEvent port name = #entity ifFalse: [^self]. 
	self panes isEmpty
		ifFalse: [
			toReplace := self panes first.
			panes := nil.
			self addPaneFromOuter: aPortEvent port.
			self announce: (GLMPaneReplaced 
						oldPane: toReplace 
						newPane: self panes first
						fromBrowser: self ) ]
		ifTrue: [
			self addPaneFromOuter: aPortEvent port.
			self announce: 
				(GLMPaneAdded 
					pane: self panes last 
					atPosition: self panes size 
					inBrowser: self)]
]

{ #category : #scripting }
GLMFinder >> passivelyFromPrevious: aPortSymbol [
	self flag: 'needs revisiting. it does not work anymore'.
	self scriptTransmission addPassiveOrigin: (GLMPort new name: aPortSymbol)
]

{ #category : #rendering }
GLMFinder >> renderGlamorouslyOn: aRenderer [ 
	^ aRenderer renderFinder: self
]

{ #category : #scripting }
GLMFinder >> showFirst: aBlock [
	^ self firstTransmission transmissionStrategy presentationsFactory: aBlock 
]

{ #category : #scripting }
GLMFinder >> variableSizePanes [
	self hasFixedSizePanes: false
]
