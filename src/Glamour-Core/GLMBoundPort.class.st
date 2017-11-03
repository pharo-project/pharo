Class {
	#name : #GLMBoundPort,
	#superclass : #GLMPort,
	#category : #Glamour-Core
}

{ #category : #comparing }
GLMBoundPort >> = anObject [ 
	"Compare this port with another for equality. 
 
	This is used to compare ParentPorts with PanePorts where the earlier is a proxy for the latter." 
	 
	^((anObject isKindOf: GLMBoundPort) and: [self pane = anObject pane]) 
		and: [self name = anObject name]
]

{ #category : #testing }
GLMBoundPort >> hasPane [ 
	 
	^self pane notNil
]

{ #category : #comparing }
GLMBoundPort >> hash [ 
	"Ports which are equal should return hash. 
 
	Since equality is used only for matching port proxies (ParentPort) with 
	their actual port, the hash function is overridden only in ParentPort." 
	 
	^super hash
]

{ #category : #validation }
GLMBoundPort >> notingPresentationChangeDo: aBlock [
	self pane notingPresentationChangeDo: aBlock
]

{ #category : #accessing }
GLMBoundPort >> pane [ 
	 
	self subclassResponsibility
]

{ #category : #validation }
GLMBoundPort >> validate: anObject [ 
	self pane presentations isEmpty ifTrue: [^true].
	^self pane presentations allSatisfy: [ :each | each validate: anObject on: self]
]
