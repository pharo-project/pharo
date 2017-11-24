"
A GLMExplicitBrowser is a browser that allows the user to explicitly define the panes and the flow of transmissions between them.
"
Class {
	#name : #GLMExplicitBrowser,
	#superclass : #GLMBrowser,
	#category : #'Glamour-Core'
}

{ #category : #accessing }
GLMExplicitBrowser >> addNewPane [
	
	^self addNewPaneNamed: 'undefined'
]

{ #category : #accessing }
GLMExplicitBrowser >> addNewPaneNamed: aString [
	^ self addPane: (GLMPane named: aString in: self)
]

{ #category : #scripting }
GLMExplicitBrowser >> custom: aPresentation [ 
	"this message sets a custom presentation to the current Transmission. 
	it is to be used within the using: block. 
	this message is particularly useful when you want to set a browser as a presentation" 
	 
	self lastTransmission addPresentation: aPresentation. 
	^ aPresentation
]

{ #category : #scripting }
GLMExplicitBrowser >> from: anOriginPortReference [ 
	"send this message after showOn: to specify an origin for the Transmission. 
	you can cascade several from: to specify additional origins" 
	 
	self lastTransmission 
		addActiveOrigin: (self resolveOriginPort: anOriginPortReference)
]

{ #category : #scripting }
GLMExplicitBrowser >> fromOutside: aPortName [
	 ^ self fromOutsidePort: aPortName
]

{ #category : #scripting }
GLMExplicitBrowser >> fromOutsidePort: aPortName [
	 ^ self from: #outer->aPortName
]

{ #category : #events }
GLMExplicitBrowser >> innerPortEvent: aPortEvent [
	| wantedTransmissions |
	aPortEvent hasChanged
		ifFalse: [ ^ self ].
	wantedTransmissions := self transmissions
		select: [ :each | (each originatesAt: aPortEvent port) and: [ (aPortEvent transmissionContext includesPort: each destination) not ] ].
	wantedTransmissions do: [ :each | each transmitIn: aPortEvent transmissionContext copy ]
]

{ #category : #accessing }
GLMExplicitBrowser >> lastTransmission [
	^ self transmissions last
]

{ #category : #accessing }
GLMExplicitBrowser >> paneNamed: aSymbol [ 
	 
	^self panes detect: [:each | each name = aSymbol]
]

{ #category : #scripting }
GLMExplicitBrowser >> passivelyFrom: anOriginPortReference [ 
	"passivelyForm: is similar to from: only the origin will be a passive one. 
	in other words, a change in the origin port will not initiate a Transmission" 
	 
	self lastTransmission
		addPassiveOrigin: (self resolveOriginPort: anOriginPortReference)
]

{ #category : #scripting }
GLMExplicitBrowser >> sendTo: aDesinationPortReference from: anOriginPortReference [ 
	"this message links two port references with a transmission that holds no presentation" 
	 
	self transmissions 
		addLast: 
			((GLMTransmission new) 
				addActiveOrigin: (self resolveOriginPort: anOriginPortReference); 
				destination: (self resolveDestinationPort: aDesinationPortReference); 
				yourself)
]

{ #category : #scripting }
GLMExplicitBrowser >> sendTo: aDesinationPortReference from: anOriginPortReference with: aBlock [ 
	"this message links two port references with a transmission that holds no presentation" 
	 
	self transmissions 
		addLast: 
			((GLMTransmission new) 
				addActiveOrigin: (self resolveOriginPort: anOriginPortReference); 
				destination: (self resolveDestinationPort: aDesinationPortReference); 
				transformation: aBlock; 
				yourself)
]

{ #category : #scripting }
GLMExplicitBrowser >> sendTo: aDesinationPortReference fromOutside: anOriginPortName [ 
	"this is a shortcut method for propagating inside an outer port name to an inner port reference" 
	 
	self 
		sendTo: aDesinationPortReference 
		from: #outer -> anOriginPortName
]

{ #category : #scripting }
GLMExplicitBrowser >> sendToOutside: aPortName from: anOriginPortReference [ 
	"this is a shortcut method for exposing an inner port reference as an outer port name" 
	 
	self 
		sendTo: #outer -> aPortName 
		from: anOriginPortReference
]

{ #category : #scripting }
GLMExplicitBrowser >> showOn: aDestinationPane [ 
	"send this message when at the beginning of specifying a Transmission" 
	 
	self addTransmission: (
		GLMTransmission new
			ensureReplacePresentationsStrategy;
			destination: (self resolveDestinationPort: aDestinationPane); 
			yourself)
]

{ #category : #accessing }
GLMExplicitBrowser >> updateContextWith: aPane [ 

]
