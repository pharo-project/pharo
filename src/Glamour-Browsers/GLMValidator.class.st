Class {
	#name : #GLMValidator,
	#superclass : #GLMImplicitBrowser,
	#instVars : [
		'buttonActions',
		'specificAnnouncementActions'
	],
	#category : #'Glamour-Browsers-Other'
}

{ #category : #accessing }
GLMValidator >> addButtonAction: aGLMGenericAction [

	self buttonActions addLast: aGLMGenericAction 
]

{ #category : #private }
GLMValidator >> addPaneFrom: aPort [ 

	self panes isEmpty ifTrue: [
		self panes addLast: (GLMPane in: self).
		self newTransmission
			addActiveOrigin: aPort; 
			destination: (self panes last port: #entity); 
			transmit
		]
	
]

{ #category : #private }
GLMValidator >> addPaneFromInner: aPort [ 
	| newTransmission realTransmissions lastActivePresentation |
	"the lastActivePresentation is needed for knowing what presentation to show by default"
	self panes isEmpty ifFalse: [lastActivePresentation := self panes last lastActivePresentation ].
	self panes addLast: ( (GLMPane in: self) lastActivePresentation: lastActivePresentation; yourself).
	self newTransmission
		addActiveOrigin: aPort; 
		destination: (self panes last port: #entity); 
		presentations: (self presentations copy); 
		transmit. 
	realTransmissions := self transmissions collect: [:each |  
			newTransmission := GLMTransmission new ensureReplacePresentationsStrategy. 
			each activeOrigins do: [:eachPort | 
				newTransmission addActiveOrigin: (aPort pane port: eachPort name)]. 
			each passiveOrigins do: [:eachPort | 
				newTransmission addPassiveOrigin: (aPort pane port: eachPort name)]. 
			newTransmission 
				destination: (self panes last port: #entity); 
				addPresentations: (self presentations collect: #copy);
				addPresentations: (each presentations collect: #copy)] . 
	(realTransmissions select: [:each | each originatesAt: aPort]) 
		do: [:each | each transmit]. 
]

{ #category : #accessing }
GLMValidator >> buttonActions [

	^ buttonActions
]

{ #category : #events }
GLMValidator >> explicitlyInnerPortNamed: aPortName [

	aPortName = #entity ifFalse: [
		self panes first ports do: [:each | (self pane port: each name) value: each value] ]
]

{ #category : #'initialize-release' }
GLMValidator >> initialize [

	super initialize.
	buttonActions := OrderedCollection new
]

{ #category : #events }
GLMValidator >> innerPortEvent: aPortEvent [ 


]

{ #category : #announcements }
GLMValidator >> on: anAnnouncementClass send: aSelector [

	self specificAnnouncementActions at: anAnnouncementClass  put: aSelector 
]

{ #category : #events }
GLMValidator >> outerPortEvent: aPortEvent [ 
	
 	super outerPortEvent: aPortEvent.
	aPortEvent port name = #entityToSelect ifTrue: [
		self tryToSelectPaneFrom: aPortEvent port ].	
	aPortEvent port name = #entity ifTrue: [
		self addPaneFrom: aPortEvent port ]
]

{ #category : #rendering }
GLMValidator >> renderGlamorouslyOn: aRenderer [ 
	^ aRenderer renderValidator: self
]

{ #category : #announcements }
GLMValidator >> setSpecificAnnouncementActionsTo: aRenderer [

	self specificAnnouncementActions keysAndValuesDo: [:anAnnouncementClass :aSymbol |
		self when: anAnnouncementClass send: aSymbol to: aRenderer.
		]
]

{ #category : #announcements }
GLMValidator >> specificAnnouncementActions [

	^ specificAnnouncementActions ifNil: [specificAnnouncementActions := Dictionary new]
]

{ #category : #accessing }
GLMValidator >> validator: aString act: aBlock [ 

	"will populate all ports by default at the end"
	self addButtonAction:   
			((GLMGenericAction new) 
				action: [:tmpBrowser |
					
					tmpBrowser panes first ports do: [:aPort |
						tmpBrowser explicitlyInnerPortNamed: aPort name
						].
					aBlock value: tmpBrowser.
					]; 
				title: aString; 
				yourself).
				
]

{ #category : #accessing }
GLMValidator >> validator: aString act: aBlock populatePorts: aCollectionOfPorts [

	self addButtonAction:   
			((GLMGenericAction new) 
				action: [:tmpBrowser |
					
					aCollectionOfPorts do: [:aSymbol |
						tmpBrowser explicitlyInnerPortNamed: aSymbol.  
 						].
					aBlock value: tmpBrowser.
					]; 
				title: aString; 
				yourself).
				
	
]

{ #category : #accessing }
GLMValidator >> validator: aString populatePorts: aCollectionOfPorts [

	self addButtonAction:   
			((GLMGenericAction new) 
				action: [:tmpBrowser |
					aCollectionOfPorts do: [:aSymbol |
						tmpBrowser explicitlyInnerPortNamed: aSymbol.  
 						]
					]; 
				title: aString; 
				yourself).
				
	
]
