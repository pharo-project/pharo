"
A SystemProgressItemMorph is 'bar' in a SystemProgressMorph. 

"
Class {
	#name : #SystemProgressItemMorph,
	#superclass : #Morph,
	#instVars : [
		'bar',
		'labelMorph',
		'lock',
		'result',
		'hasResult',
		'startValue',
		'endValue',
		'lastRefresh'
	],
	#classVars : [
		'BarHeight',
		'BarWidth'
	],
	#category : #Morphic-Base-ProgressBar
}

{ #category : #'instance creation' }
SystemProgressItemMorph class >> labeled: aString from: startValue to: endValue [

	^ self new initializeLabel: aString start: startValue end: endValue.
]

{ #category : #API }
SystemProgressItemMorph >> beComplete [

	self close.
]

{ #category : #API }
SystemProgressItemMorph >> changed [

	| msRefreshRate isTimeForRefresh |
	msRefreshRate := 25.
	isTimeForRefresh := Time millisecondClockValue - lastRefresh >= msRefreshRate.
	super changed.
	(self isInWorld and: [ isTimeForRefresh ]) ifTrue: [ self refresh ].
]

{ #category : #private }
SystemProgressItemMorph >> close [

	SystemProgressMorph uniqueInstance close: self.
]

{ #category : #API }
SystemProgressItemMorph >> current [

	^ bar value.
]

{ #category : #API }
SystemProgressItemMorph >> current: aNumber [

	bar value: aNumber.
	self changed.
]

{ #category : #API }
SystemProgressItemMorph >> decrement [

	bar decrement.
]

{ #category : #private }
SystemProgressItemMorph >> do: aBlock [

	self changed. "We may be in the UI thread, so this will give us a change to dsiplay ourselves before the block starts"
	^ [ aBlock value: self ] ensure: [ self close ].
]

{ #category : #private }
SystemProgressItemMorph >> forceRefreshOnNextChange [

	lastRefresh := 0.
]

{ #category : #API }
SystemProgressItemMorph >> increment [

	bar increment.
]

{ #category : #private }
SystemProgressItemMorph >> initializeLabel: aString start: startNumber end: endNumber [
	
	lock := Semaphore forMutualExclusion.
	hasResult := false.
	lastRefresh := 0.
	
	self
		color: Color transparent;
		layoutPolicy: TableLayout new;
		listDirection: #topToBottom;
		cellPositioning: #center;
		cellInset: 5;
		listCentering: #center;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap.
		
	labelMorph := StringMorph contents: aString font: StandardFonts windowTitleFont.
	self addMorphBack: labelMorph.

	bar := ProgressBarMorph from: startNumber to: endNumber.
	self addMorphBack: bar.
]

{ #category : #API }
SystemProgressItemMorph >> label [
	
	^ self labelMorph contents.
]

{ #category : #API }
SystemProgressItemMorph >> label: aString [

	self label isEmpty ifTrue: [ self forceRefreshOnNextChange ].
	self labelMorph contents: aString.
	self changed.
]

{ #category : #private }
SystemProgressItemMorph >> labelMorph [

	^ labelMorph.
]

{ #category : #private }
SystemProgressItemMorph >> max: aNumber [
 
	endValue := aNumber.
]

{ #category : #private }
SystemProgressItemMorph >> refresh [

	lastRefresh := Time millisecondClockValue.
	UIManager default uiProcess == Processor activeProcess
		ifTrue: [ self currentWorld doOneCycleNow ]
]

{ #category : #private }
SystemProgressItemMorph >> result [

	^ result.
]

{ #category : #private }
SystemProgressItemMorph >> result: anObject [

	lock critical: [ 
		hasResult := true.
		result := anObject ].
]

{ #category : #private }
SystemProgressItemMorph >> start: aNumber [

	startValue := aNumber.
]

{ #category : #private }
SystemProgressItemMorph >> startAt: aNumber [
 
	startValue := aNumber.
]
