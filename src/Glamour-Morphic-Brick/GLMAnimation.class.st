"
A GLMAnimation class is a basic animation class that works in background thread and performs brick transformation in UI thread
"
Class {
	#name : #GLMAnimation,
	#superclass : #Object,
	#instVars : [
		'asyncTask',
		'duration',
		'animationLogic',
		'startTime',
		'fromLogic',
		'fromValue',
		'toLogic',
		'toValue',
		'currentValue',
		'animatedObject',
		'stepLogic',
		'doStep',
		'shouldStop',
		'callback',
		'steppedLogic',
		'isCompleted',
		'ensureLogic',
		'stamp'
	],
	#category : #'Glamour-Morphic-Brick-Async'
}

{ #category : #'instance creation' }
GLMAnimation class >> borderColor [

	^ GLMAnimation new
		step: [ :brick :from :to :delta |
			|r g b a|
			r := (to red - from red) * delta + from red.
			g := (to green - from green) * delta + from green.
			b := (to blue - from blue) * delta + from blue.
			a := (to alpha - from alpha) * delta + from alpha.
			Color r: r g: g b: b alpha: a ];
		animation: [ :b :v | b borderColor: v. b changed ]
]

{ #category : #sources }
GLMAnimation class >> exampleSimple [
	|brick|

	brick := GLMLabelBrick new
		color: Color lightBlue;
		textPosition: #center.

	GLMAnimation new
		linear;
		duration: 5000;
		from: 50@50;
		to: 400@400;
		animation: [ :b :v | b extent: v ];
		onStepped: [ :b :delta | b text: ((delta * 100) asInteger asString, '%') ];
		onCompleted: [ :b | b invalidateWith: [ b color: Color lightGreen; text: 'Done! 100%' ] ];
		animate: brick.
	
	brick asBrickedMorph
		extent: 400@400;
		openInWindow
]

{ #category : #'instance creation' }
GLMAnimation class >> swipeLeft [

	^ GLMAnimation new
		linear;
		duration: 250;
		from: 0;
		to: -1.2;
		animation: [ :b :v | b changed. b translateXTo: (v * b width). b changed ]
]

{ #category : #scripting }
GLMAnimation >> animate: anObject [

	self asyncTask terminate.
	animatedObject := anObject.
	self reset.
	
	stamp := Time millisecondClockValue.
	self asyncTask
		doInBackground: [ [ self shouldStop ] whileFalse: [
				self needsStep ifTrue: [ self animationStep ] ] ];
		onCompleted: [ 
			isCompleted := true.
			[ callback brickValue: self animatedObject ]
				ensure: [ ensureLogic brickValue: self animatedObject ] ];
		priority: Processor userSchedulingPriority;
		execute
]

{ #category : #accessing }
GLMAnimation >> animatedObject [

	^ animatedObject
]

{ #category : #scripting }
GLMAnimation >> animation: anObject [

	animationLogic := anObject
]

{ #category : #animating }
GLMAnimation >> animationStep [
	|delta stepValue currentTime stop stepStamp|
	self needsStep: false.
	currentTime := Time millisecondClockValue.
	stop := currentTime >= self finishTime.
	delta := self duration = 0
		ifTrue: [ 1 ]
		ifFalse: [ ((currentTime - startTime) min: self duration) / self duration ].
	delta = 1 ifTrue: [ stop := true ].
	stepValue := stepLogic
		brickValue: self animatedObject
		withEnoughArguments: { self fromValue . self toValue . delta }.
	(delta = 1) | (stepValue = self toValue) ifTrue: [ stop := true ].
	stepStamp := stamp.
	UIManager default defer: [
		self asyncTask isTerminated not & (stepStamp = stamp) ifTrue: [
			steppedLogic brickValue: self animatedObject withEnoughArguments: { delta }.
			animationLogic brickValue: self animatedObject withEnoughArguments: { stepValue }.
			stop
				ifFalse: [ self needsStep: true. self asyncTask resume ]
				ifTrue: [ self shouldStop: true. self asyncTask resume ] ] ].
		
	self asyncTask pause
	
	
]

{ #category : #accessing }
GLMAnimation >> asyncTask [

	^ asyncTask ifNil: [ asyncTask := GLMAsyncTask new ]
]

{ #category : #accessing }
GLMAnimation >> asyncTask: anObject [
	asyncTask := anObject
]

{ #category : #accessing }
GLMAnimation >> duration [

	^ duration ifNil: [ duration := 0 ]
]

{ #category : #scripting }
GLMAnimation >> duration: anInteger [

	duration := anInteger
]

{ #category : #scripting }
GLMAnimation >> ensure: aBlock [

	ensureLogic := aBlock
]

{ #category : #accessing }
GLMAnimation >> finishTime [

	^ self startTime + self duration
]

{ #category : #scripting }
GLMAnimation >> from: anObject [

	fromLogic := anObject
]

{ #category : #accessing }
GLMAnimation >> fromValue [

	^ fromValue ifNil: [ fromValue := fromLogic brickValue: self animatedObject ]
]

{ #category : #accessing }
GLMAnimation >> isCompleted [

	^ isCompleted ifNil: [ isCompleted := false ]
]

{ #category : #scripting }
GLMAnimation >> linear [

	self step: [ :b :from :to :delta | (to - from) * delta + from ]
]

{ #category : #accessing }
GLMAnimation >> needsStep [

	^ doStep ifNil: [ doStep := true ]
]

{ #category : #accessing }
GLMAnimation >> needsStep: aBoolean [

	doStep := aBoolean
]

{ #category : #scripting }
GLMAnimation >> onCompleted: aBlock [

	callback := aBlock
]

{ #category : #scripting }
GLMAnimation >> onStepped: aBlock [

	steppedLogic := aBlock
]

{ #category : #initialization }
GLMAnimation >> reset [

	isCompleted := false.
	shouldStop := false.
	doStep := true.
	fromValue := nil.
	toValue := nil.
	currentValue := nil.
	startTime := nil
]

{ #category : #accessing }
GLMAnimation >> shouldStop [

	 ^ shouldStop ifNil: [ shouldStop := false ]
]

{ #category : #accessing }
GLMAnimation >> shouldStop: aBoolean [

	shouldStop := aBoolean
]

{ #category : #accessing }
GLMAnimation >> startTime [

	^ startTime ifNil: [ startTime := Time millisecondClockValue ]
]

{ #category : #scripting }
GLMAnimation >> step: anObject [

	stepLogic := anObject
]

{ #category : #actions }
GLMAnimation >> terminate [

	self asyncTask terminate.
	(self isCompleted not & self animatedObject isNotNil) ifTrue: [
		ensureLogic brickValue: self animatedObject ]
]

{ #category : #scripting }
GLMAnimation >> to: anObject [

	toLogic := anObject
]

{ #category : #accessing }
GLMAnimation >> toValue [

	^ toValue ifNil: [ toValue := toLogic brickValue: self animatedObject ]
]
