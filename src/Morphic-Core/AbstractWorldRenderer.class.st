"
I implement the logic to render a world.
I have different implementations to start the world.
"
Class {
	#name : 'AbstractWorldRenderer',
	#superclass : 'Object',
	#instVars : [
		'world',
		'alreadyActivated'
	],
	#classVars : [
		'IsLoggingRenderingProfileToFile',
		'IsProfilingRenderingTime',
		'MainWorldRenderer',
		'RenderingProfilingLogOutput'
	],
	#category : 'Morphic-Core-Worlds',
	#package : 'Morphic-Core',
	#tag : 'Worlds'
}

{ #category : 'profiling' }
AbstractWorldRenderer class >> closeRenderingProfileLogFile [

	RenderingProfilingLogOutput ifNotNil: [
		RenderingProfilingLogOutput close.
		RenderingProfilingLogOutput := nil ]
]

{ #category : 'accessing' }
AbstractWorldRenderer class >> detectCorrectOneForWorld: aWorld [

	| aRenderer |
	MainWorldRenderer ifNotNil: [ ^ MainWorldRenderer ].

	(self allSubclasses sorted: [ :a :b | a priority > b priority ])
		detect: [ :aClass | aClass isApplicableFor: aWorld ]
		ifFound: [ :aClass | aRenderer := aClass forWorld: aWorld ].

	^ MainWorldRenderer := aRenderer
]

{ #category : 'profiling' }
AbstractWorldRenderer class >> disableRenderingProfileLogFile [
	<script>

	self isLoggingRenderingProfileToFile: false
]

{ #category : 'profiling' }
AbstractWorldRenderer class >> enableRenderingProfileLogFile [
	<script>

	self isLoggingRenderingProfileToFile: true
]

{ #category : 'instance creation' }
AbstractWorldRenderer class >> forWorld: aWorld [

	^ self new
		  world: aWorld;
		  yourself
]

{ #category : 'class initialization' }
AbstractWorldRenderer class >> initialize [

	SessionManager default registerSystemClassNamed: self name
]

{ #category : 'testing' }
AbstractWorldRenderer class >> isAbstract [

	^ self == AbstractWorldRenderer
]

{ #category : 'testing' }
AbstractWorldRenderer class >> isApplicableFor: aWorld [

	^ self subclassResponsibility
]

{ #category : 'profiling - testing' }
AbstractWorldRenderer class >> isLoggingRenderingProfileToFile [

	^ IsLoggingRenderingProfileToFile ifNil: [ IsLoggingRenderingProfileToFile := false ]
]

{ #category : 'profiling - testing' }
AbstractWorldRenderer class >> isLoggingRenderingProfileToFile: aBoolean [

	IsLoggingRenderingProfileToFile := aBoolean.
	IsLoggingRenderingProfileToFile ifFalse: [ self closeRenderingProfileLogFile ]
]

{ #category : 'profiling - testing' }
AbstractWorldRenderer class >> isProfilingRenderingTime [

	^ IsProfilingRenderingTime ifNil: [ IsProfilingRenderingTime := false ]
]

{ #category : 'profiling - testing' }
AbstractWorldRenderer class >> isProfilingRenderingTime: aBoolean [
	"
	self isProfilingRenderingTime: true.
	self isProfilingRenderingTime: false.
	"

	IsProfilingRenderingTime := aBoolean.
	IsProfilingRenderingTime ifFalse: [ self closeRenderingProfileLogFile ]
]

{ #category : 'profiling' }
AbstractWorldRenderer class >> logRenderingTimeMeasurements: measurements [
	self isLoggingRenderingProfileToFile ifFalse: [ ^ self ].
	RenderingProfilingLogOutput ifNil: [
		RenderingProfilingLogOutput := (FileSystem workingDirectory / 'rendering-profile-log.csv') writeStream.
		RenderingProfilingLogOutput truncate.
		RenderingProfilingLogOutput nextPutAll: 'Class,Measurement,Time'; lf.
	].
	measurements do: [ :each |
		RenderingProfilingLogOutput nextPutAll: self name asString;
			nextPut: $,; nextPutAll: each key asString;
			nextPut: $,; nextPutAll: each value asString;
			lf
	]
]

{ #category : 'accessing' }
AbstractWorldRenderer class >> mainWorldRenderer [

	^ MainWorldRenderer
]

{ #category : 'accessing' }
AbstractWorldRenderer class >> priority [

	^ 0
]

{ #category : 'system startup' }
AbstractWorldRenderer class >> shutDown: quitting [

	MainWorldRenderer ifNotNil: [ :aRenderer |
		aRenderer shutDown: quitting ].

	quitting ifFalse: [ ^ self ].
	MainWorldRenderer := nil
]

{ #category : 'profiling' }
AbstractWorldRenderer class >> startProfilingRenderingTime [
	<script>

	self isProfilingRenderingTime: true
]

{ #category : 'system startup' }
AbstractWorldRenderer class >> startUp: isANewSession [

	isANewSession ifTrue: [ MainWorldRenderer := nil ].
	MainWorldRenderer ifNotNil: [
		MainWorldRenderer startUp: isANewSession ]
]

{ #category : 'profiling' }
AbstractWorldRenderer class >> stopProfilingRenderingTime [
	<script>

	self isProfilingRenderingTime: false
]

{ #category : 'activation' }
AbstractWorldRenderer >> activate [

	alreadyActivated ifTrue: [
		"Maybe I need to change the title of the window after saving the image"
		^ self updateWindowTitle ].

	self doActivate.
	alreadyActivated := true
]

{ #category : 'activation' }
AbstractWorldRenderer >> actualScreenSize [

	self subclassResponsibility
]

{ #category : 'events' }
AbstractWorldRenderer >> checkForNewScreenSize [

	self subclassResponsibility
]

{ #category : 'utilities' }
AbstractWorldRenderer >> convertWindowMouseEventPosition: aPosition [

	self subclassResponsibility
]

{ #category : 'activation' }
AbstractWorldRenderer >> deactivate [

	self subclassResponsibility
]

{ #category : 'operations' }
AbstractWorldRenderer >> deferUpdates: aBoolean [

	^ false
]

{ #category : 'rendering' }
AbstractWorldRenderer >> deferUpdatesDuring: aBlock [

	^ aBlock value
]

{ #category : 'rendering' }
AbstractWorldRenderer >> displayWorldState: aWorldState ofWorld: aWorld [
	"Update this world's display."

	| submorphs |
	submorphs := aWorld submorphs.

	"force re-layout if needed"
	submorphs do: [ :m | m fullBounds ].

	"display is already up-to-date"
	aWorldState checkIfUpdateNeeded
		ifFalse: [ ^ self ].

	self deferUpdatesDuring: [
		self drawDuring: [ :aMorphicCanvas | | worldDamageRects handDamageRects allDamage handsToDraw |
			worldDamageRects := aWorldState drawWorld: aWorld submorphs: submorphs invalidAreasOn: aMorphicCanvas.

			"repair world's damage on canvas"
			handsToDraw := aWorldState selectHandsToDrawForDamage: worldDamageRects.
			handDamageRects := handsToDraw collect: [:h | h savePatchFrom: aMorphicCanvas].
			allDamage := worldDamageRects, handDamageRects.

			"draw hands onto world canvas"
			handsToDraw reverseDo: [:h | aMorphicCanvas fullDrawMorph: h].

			aMorphicCanvas finish.
			self updateDamage: allDamage.

			"restore world canvas under hands"
	 		handsToDraw do: [:h | h restoreSavedPatchOn: aMorphicCanvas ].
		] ]
]

{ #category : 'activation' }
AbstractWorldRenderer >> doActivate [

	self subclassResponsibility
]

{ #category : 'rendering' }
AbstractWorldRenderer >> drawDuring: aBlock [

	self subclassResponsibility
]

{ #category : 'profiling' }
AbstractWorldRenderer >> formatRenderingTimeMeasurement: measurement [

	^ '[{1}]{2}: {3} ms' format: {
			  self class name.
			  measurement key.
			  (measurement value printPaddedWith: Character space to: 5) }
]

{ #category : 'operations' }
AbstractWorldRenderer >> fullscreenMode: aValue [
	"We ignore the value by default - subclasses might handle fullscreen mode"
]

{ #category : 'accessing' }
AbstractWorldRenderer >> icon: aForm [

	self subclassResponsibility
]

{ #category : 'initialization' }
AbstractWorldRenderer >> initialize [

	alreadyActivated := false
]

{ #category : 'rendering' }
AbstractWorldRenderer >> invalidate [
	"Invalidate the entire render"
	"Each subclass should define me properly"
]

{ #category : 'testing' }
AbstractWorldRenderer >> isProfilingRenderingTime [
	^ self class isProfilingRenderingTime
]

{ #category : 'profiling' }
AbstractWorldRenderer >> logRenderingTimeMeasurements: measurements [
	self class logRenderingTimeMeasurements: measurements
]

{ #category : 'events' }
AbstractWorldRenderer >> requestStopTextEditing [
	^ self subclassResponsibility
]

{ #category : 'events' }
AbstractWorldRenderer >> requestTextEditingAt: aRectangle [

	self subclassResponsibility
]

{ #category : 'operations' }
AbstractWorldRenderer >> restoreMorphicDisplay [

	world
		extent: self actualScreenSize;
		viewBox: self viewBox;
		handsDo: [:h | h visible: true; showTemporaryCursor: nil];
		resizeBackgroundMorph;
		submorphsDo: [ :each | each displayExtentChanged ];
		fullRepaintNeeded.

	world defer: [Cursor normal show].

	world layoutChanged
]

{ #category : 'system startup' }
AbstractWorldRenderer >> shutDown: quitting [

	quitting ifTrue: [ self deactivate ]
]

{ #category : 'system startup' }
AbstractWorldRenderer >> startUp: resuming [
]

{ #category : 'rendering' }
AbstractWorldRenderer >> updateDamage: rectangles [

	self subclassResponsibility
]

{ #category : 'events' }
AbstractWorldRenderer >> updateToNewResolution [

	self subclassResponsibility
]

{ #category : 'activation' }
AbstractWorldRenderer >> updateWindowTitle [

	"I do nothing... maybe my subclasses extend it"
]

{ #category : 'accessing' }
AbstractWorldRenderer >> usableArea [

	^ self viewBox
]

{ #category : 'display box access' }
AbstractWorldRenderer >> viewBox [

	^ 0@0 corner: self actualScreenSize
]

{ #category : 'accessing' }
AbstractWorldRenderer >> windowTitle: aString [

	self subclassResponsibility
]

{ #category : 'accessing' }
AbstractWorldRenderer >> world [
	^ world
]

{ #category : 'accessing' }
AbstractWorldRenderer >> world: anObject [
	world := anObject
]
