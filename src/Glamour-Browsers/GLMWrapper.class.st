"
The GLMWrapper is a browser that has only one pane and that is typically used as a placeholder.

The pane takes only entity as input.
"
Class {
	#name : #GLMWrapper,
	#superclass : #GLMImplicitBrowser,
	#category : #'Glamour-Browsers-Other'
}

{ #category : #'initialize-release' }
GLMWrapper >> addDefaultPane [
	self addPane: (GLMPane named: #default)
]

{ #category : #accessing }
GLMWrapper >> defaultPane [ 
	^ self panes anyOne 
]

{ #category : #'initialize-release' }
GLMWrapper >> initialize [
	super initialize.
	self addDefaultPane.
]

{ #category : #events }
GLMWrapper >> innerPortEvent: aPortEvent [ 
	"We forward all inner ports to the outside world"
	aPortEvent portName = #entity ifFalse: [
		"We don't want to resend the transmission, but use a new SimpleTransmission." 
		(self pane port: aPortEvent portName) value: aPortEvent value]
]

{ #category : #events }
GLMWrapper >> outerPortEvent: aPortEvent [ 
	"We only treat the #entity port, and for it we simply trigger 
	a transmission to the default pane"

	super outerPortEvent: aPortEvent.
	aPortEvent port name = #entity ifFalse: [^self].
	self newTransmission
		addActiveOrigin: (aPortEvent port); 
		destination: (self panes anyOne port: #entity); 
		transmit
]

{ #category : #rendering }
GLMWrapper >> renderGlamorouslyOn: aRenderer [ 
	^ aRenderer renderWrapper: self
]
