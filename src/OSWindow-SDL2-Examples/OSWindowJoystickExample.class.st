"
I am a basic example to show how we can draw something and use the joystick.

To launch the example:
self new open
"
Class {
	#name : #OSWindowJoystickExample,
	#superclass : #OSWindowGenericRendererExample,
	#instVars : [
		'characterPosition',
		'characterVelocity',
		'lastSimulationTime',
		'currentTime',
		'bullets'
	],
	#category : #'OSWindow-SDL2-Examples-Joystick'
}

{ #category : #accessing }
OSWindowJoystickExample >> characterPosition [
	^ characterPosition
]

{ #category : #accessing }
OSWindowJoystickExample >> characterPosition: anObject [
	characterPosition := anObject
]

{ #category : #accessing }
OSWindowJoystickExample >> characterVelocity [
	^ characterVelocity
]

{ #category : #accessing }
OSWindowJoystickExample >> characterVelocity: anObject [
	characterVelocity := anObject
]

{ #category : #factory }
OSWindowJoystickExample >> createEventHandler [
	^ OSWindowJoystickExampleEventHandler new 
		example: self
]

{ #category : #drawing }
OSWindowJoystickExample >> drawBullets [
	renderer
		color: (Color r: (currentTime * 2.0) cos * 0.4 + 0.6 g: (currentTime * 3.0) sin * 0.4 + 0.6 b: 0.0).
	bullets do: [ :bullet |
		renderer fillRectangle: (bullet first extent: 10@10)
	]
]

{ #category : #drawing }
OSWindowJoystickExample >> drawCharacter [
	renderer
		color: (Color r: 0.0 g: (currentTime*2.0) cos * 0.3 + 0.5 b: 1.0);
		fillRectangle: (characterPosition extent: 20@40)
]

{ #category : #initialization }
OSWindowJoystickExample >> initialize [
	super initialize.
	bullets := OrderedCollection new.
	self reset.
]

{ #category : #drawing }
OSWindowJoystickExample >> redraw [
	self simulate.
	renderer
		color: Color black;
		clear.
		
	self
		drawCharacter;
		drawBullets
]

{ #category : #initialization }
OSWindowJoystickExample >> reset [
	characterPosition := 320@240.
	characterVelocity := 0@0.
	bullets removeAll.
]

{ #category : #'game controls' }
OSWindowJoystickExample >> shoot [
	| position velocity |
	position := characterPosition.
	velocity := 400 @ characterVelocity y.
	bullets add: { position . velocity }
]

{ #category : #simulation }
OSWindowJoystickExample >> simulate [
	| deltaTime |
	lastSimulationTime ifNil: [ lastSimulationTime := Time millisecondClockValue * 0.001].
	currentTime := Time millisecondClockValue * 0.001.
	deltaTime := currentTime - lastSimulationTime.
	lastSimulationTime := currentTime.
	
	self
		simulateCharacter: deltaTime;
		simulateBullets: deltaTime.
]

{ #category : #simulation }
OSWindowJoystickExample >> simulateBullets: delta [
	| position velocity |
	
	bullets do: [ :bullet |
		position := bullet first.
		velocity	:= bullet second.
		position := position + (velocity * delta).
		bullet at: 1 put: position.
	].
]

{ #category : #simulation }
OSWindowJoystickExample >> simulateCharacter: deltaTime [
	characterPosition := characterPosition + (characterVelocity * deltaTime).
]
