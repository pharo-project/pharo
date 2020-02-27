"
I display a set of progress indicator in a list.
Special care is taken to update the view while the rest of the UI process is blocked.

Reset.
SystemProgressMorph reset.

Examples.
self show: 'Doing...' from: 500 to: 600 during: [ :bar |
	500 to: 600 do: [ :x | 
		bar current: x.
		(Delay forMilliseconds: 20) wait 
		""Just to slow it down so we can see what's going on"" ] ].

self show: 'Doing...' from: 0 to: 50 during: [ :bar |
	0 to: 50 do: [ :x | 
		bar increment.
		bar label: x asString.
		(Delay forMilliseconds: 20) wait ""Just to slow it down so we can see what's going on"" ] ].
"
Class {
	#name : #SystemProgressMorph,
	#superclass : #BorderedMorph,
	#traits : 'TAbleToRotate',
	#classTraits : 'TAbleToRotate classTrait',
	#instVars : [
		'lock',
		'lastRefresh'
	],
	#classVars : [
		'UniqueInstance'
	],
	#classInstVars : [
		'horizontalPosition',
		'verticalPosition'
	],
	#category : #'Morphic-Base-ProgressBar'
}

{ #category : #cleanup }
SystemProgressMorph class >> cleanUp [
	self reset
]

{ #category : #'enabling/disabling' }
SystemProgressMorph class >> disable [

	Job jobAnnouncer unsubscribe: self.
]

{ #category : #'enabling/disabling' }
SystemProgressMorph class >> enable [

	Job jobAnnouncer when: JobStart send: #startJob: to: self.
	Job jobAnnouncer when: JobEnd send: #endJob: to: self.
	Job jobAnnouncer when: JobChange send: #updateJob: to: self.

]

{ #category : #'job subscription' }
SystemProgressMorph class >> endJob: aJobEnd [
	self flag: #pharoFixMe.	"camillo I need help"
	self uniqueInstance bars ifNotEmpty: [ :bars | self uniqueInstance close: bars last ]
]

{ #category : #examples }
SystemProgressMorph class >> example [
	"SystemProgressMorph example"
	'Progress' 
		displayProgressFrom: 0 to: 1000
		during: [:bar | 0 to: 1000 do: [:i | bar current: i. (Delay forMilliseconds: 2) wait]]

]

{ #category : #examples }
SystemProgressMorph class >> exampleChangeLabel [
	"SystemProgressMorph exampleChangeLabel"
	| classes |
	classes := Smalltalk globals allClasses copyFrom: 1 to: 100.
	'InitialLabel' 
		displayProgressFrom: 0 to: classes size
		during: [:bar | 1 to: classes size do: [:i |
				bar current: i.
				bar label: i printString, '/', classes size printString, ' ', (classes at: i) printString.
				(Delay forMilliseconds: 100) wait]]

]

{ #category : #examples }
SystemProgressMorph class >> exampleLabelOnly [
	"SystemProgressMorph exampleLabelOnly"
	| words |
	words := #(zero one two three four five six seven eight nine ten) reversed.
	UIManager default informUserDuring: [:bar |
		words do: [:each|
			bar label: 'Countdown: ', each.
			(Delay forSeconds: 1) wait]].
]

{ #category : #accessing }
SystemProgressMorph class >> horizontalPosition [
	^ horizontalPosition ifNil: [ horizontalPosition := 0 ]
]

{ #category : #accessing }
SystemProgressMorph class >> horizontalPosition: aFloat [
	"0.0: left ... 1.0: right"
	horizontalPosition := aFloat.
	self uniqueInstance reposition.
]

{ #category : #initialization }
SystemProgressMorph class >> initialize [
	"SystemProgressMorph initialize"

	self reset.
]

{ #category : #initialization }
SystemProgressMorph class >> reset [
	"SystemProgressMorph reset"
	UniqueInstance ifNotNil: [ UniqueInstance delete ].
	UniqueInstance := nil.
]

{ #category : #settings }
SystemProgressMorph class >> settingOn: aBuilder [
	<systemsettings>
	(aBuilder group: #progress)
		label: 'Progress Bar';
		description: 'Progress Bar settings';
		parent: #morphic;
		with: [
			(aBuilder pickOne: #horizontalPosition)
				target: self;
				getSelector: #horizontalPosition;
				setSelector: #horizontalPosition:;
				label: 'Horizontal progress indicator position';
				domainValues: {'left'->0. 'middle'->0.5. 'right'->1}.
							
			(aBuilder pickOne: #verticalPosition)
				target: self;
				getSelector: #verticalPosition;
				setSelector: #verticalPosition:;
				default: 0;
				label: 'Vertical progress indicator position';
				domainValues: {'bottom'->1. 'middle'->0.5. 'top'->0}]
]

{ #category : #'instance creation' }
SystemProgressMorph class >> show: aJob [

	^ self uniqueInstance show: aJob.
]

{ #category : #'instance creation' }
SystemProgressMorph class >> show: aString from: startNumber to: endNumber [
	self flag: #pharoFixMe.	"When we go from Exceptions to announcements, delete this and use show:from:to:during:"

	^ self uniqueInstance show: aString from: startNumber to: endNumber
]

{ #category : #'instance creation' }
SystemProgressMorph class >> show: aString from: startNumber to: endNumber during: aBlock [
	self flag: #pharoFixMe.	"When we go from Exceptions to announcements, this will be the primary entry point"

	^ self uniqueInstance
		show: aString
		from: startNumber
		to: endNumber
		during: aBlock
]

{ #category : #'job subscription' }
SystemProgressMorph class >> startJob: aJobStart [

	self show: aJobStart job
]

{ #category : #'instance creation' }
SystemProgressMorph class >> uniqueInstance [

	^ UniqueInstance ifNil: [ UniqueInstance := self new ].
]

{ #category : #'job subscription' }
SystemProgressMorph class >> updateJob: aJobChange [
	| bars |
	bars := self uniqueInstance bars.
	bars isEmpty 
		ifFalse: [ 
			bars last 
				label: aJobChange title;
				progress: aJobChange progress ]
]

{ #category : #accessing }
SystemProgressMorph class >> verticalPosition [
	^ verticalPosition ifNil: [ verticalPosition := 0 ]
]

{ #category : #accessing }
SystemProgressMorph class >> verticalPosition: fraction [
	"0.0: top ... 1.0: bottom"
	verticalPosition := fraction.
	self uniqueInstance reposition.
]

{ #category : #private }
SystemProgressMorph >> addItemShowing: aJob [
	
	lock critical: [ | item items |
		item := JobProgressMorph job: aJob.
		items := self bars size.
		items < 10 ifTrue: [ 
			self addMorphBack: item.
			self resize ].
		^ item ].
]

{ #category : #private }
SystemProgressMorph >> addItemShowing: aString from: startNumber to: endNumber [
	
	lock critical: [ | item items |
		item := SystemProgressItemMorph labeled: aString from: startNumber to: endNumber.
		items := self bars size.
		items < 10 ifTrue: [ 
			self addMorphBack: item.
			self recenter ].
		^ item ].
]

{ #category : #private }
SystemProgressMorph >> bars [
	
	^ self submorphs.
]

{ #category : #private }
SystemProgressMorph >> close: aSystemProgressItemMorph [

	self bars isEmpty ifTrue: [ ^ self ].
	
	lock critical: [
		aSystemProgressItemMorph delete.
		self bars size = 0 ifTrue: [ 
			self width: 0.
			self delete ] ].

	self refresh.
]

{ #category : #'submorphs-add/remove' }
SystemProgressMorph >> dismissViaHalo [

	self class reset.
]

{ #category : #initialization }
SystemProgressMorph >> initialize [

	super initialize.
	lock := Semaphore forMutualExclusion.
	lastRefresh := 0.
	self
		setDefaultParameters;
		setProperty: #morphicLayerNumber toValue: self morphicLayerNumber;
		layoutPolicy: TableLayout new;
		listDirection: #topToBottom;
		cellPositioning: #topCenter;
		cellInset: 5;
		listCentering: #center;
		hResizing: #rigid;
		vResizing: #shrinkWrap;
		layoutInset: 10@6;
		minWidth: 150
]

{ #category : #accessing }
SystemProgressMorph >> lastRefresh [

        ^ lastRefresh ifNil: [ lastRefresh := 0 ]
]

{ #category : #private }
SystemProgressMorph >> maxBarWidth [

	^ self bars inject: 0 into: [ :max :next | next minExtent x max: max ].
]

{ #category : #initialization }
SystemProgressMorph >> morphicLayerNumber [
	"progress morphs are behind menus and balloons, but in front of most other stuff"
	^self valueOfProperty: #morphicLayerNumber ifAbsent: [ 12 ].

]

{ #category : #display }
SystemProgressMorph >> openInWorld [

	super openInWorld.
	self width: 200.
	self resize.
	
]

{ #category : #updating }
SystemProgressMorph >> recenter [
	self reposition
]

{ #category : #updating }
SystemProgressMorph >> refresh [
	"We may be blocking the UI thread, and thus have to draw the world ourselves when necessary"
	lastRefresh := Time millisecondClockValue.
	UIManager default uiProcess == Processor activeProcess
		ifTrue: [ self currentWorld doOneCycleNow]
]

{ #category : #private }
SystemProgressMorph >> reposition [
	"Calculate the proper position"
	
	| xPos yPos margin |
	margin := 20.
	
	xPos := Display boundingBox width - self fullBounds width - (2*margin).
	xPos := xPos * self class horizontalPosition + margin.
	
	yPos := Display boundingBox height - self fullBounds height - (2*margin).
	yPos := yPos * self class verticalPosition + margin.
	
	self align: self fullBounds topLeft with: Display boundingBox topLeft + (xPos @ yPos).
	
	self refresh.
]

{ #category : #updating }
SystemProgressMorph >> resize [

	| newWidth |
	newWidth := self maxBarWidth + 50 max: self width.
	self width: newWidth.
]

{ #category : #initialization }
SystemProgressMorph >> setDefaultParameters [

	self theme setSystemProgressMorphDefaultParameters: self
]

{ #category : #private }
SystemProgressMorph >> show: aJob [
	| progressMorph |
	self flag: #pharoFixMe.	"When we go from Exceptions to announcements, delete this and use show:from:to:during:"

	progressMorph := self addItemShowing: aJob.

	self openInWorld.
	self updateWidth.

	^ progressMorph
]

{ #category : #private }
SystemProgressMorph >> show: aString from: startNumber to: endNumber [
	| progressMorph |
	self flag: #pharoFixMe.	"When we go from Exceptions to announcements, delete this and use show:from:to:during:"

	self openInWorld.
	progressMorph := self addItemShowing: aString from: startNumber to: endNumber.

	self
		refresh;
		reposition.

	^ progressMorph
]

{ #category : #private }
SystemProgressMorph >> show: aString from: startNumber to: endNumber during: aBlock [

	| progressMorph result |

	self openInWorld.
	progressMorph := self addItemShowing: aString from: startNumber to: endNumber.
		
	self 
		refresh;
		reposition.

	[ result := progressMorph do: aBlock ] ensure: [ self close: progressMorph ].
	^ result.
]

{ #category : #updating }
SystemProgressMorph >> update: aSymbol [
	
	| msRefreshRate isTimeForRefresh |
	
	aSymbol == #width
		ifTrue: [ self updateWidth ].
		
	
	msRefreshRate := 60 "roughly 16 times per second".
	isTimeForRefresh := Time millisecondClockValue - self lastRefresh >= msRefreshRate.
	(self isInWorld and: [ isTimeForRefresh ]) ifFalse: [ ^ self ].
	
	self refresh
]

{ #category : #updating }
SystemProgressMorph >> updateColor [
	"Callback from theme"
	
	self theme preferGradientFill ifFalse: [^ self].
	self fillStyle: (self theme progressFillStyleFor: self)
]

{ #category : #updating }
SystemProgressMorph >> updateProgressValue [


]

{ #category : #updating }
SystemProgressMorph >> updateWidth [

	self resize
]
