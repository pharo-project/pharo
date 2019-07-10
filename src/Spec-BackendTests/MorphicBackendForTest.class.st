Class {
	#name : #MorphicBackendForTest,
	#superclass : #AbstractBackendForTest,
	#instVars : [
		'uiWaitingSemaphore',
		'app'
	],
	#category : #'Spec-BackendTests'
}

{ #category : #asserting }
MorphicBackendForTest >> assertHasHeader: aFTTableMorph [
	self waitUntilUIRedrawed.

	self assert: (aFTTableMorph instVarNamed: #showColumnHeaders)
]

{ #category : #asserting }
MorphicBackendForTest >> assertIcon: anAlphaImageMorph equals: aForm [ 
	
	self assert: (anAlphaImageMorph ifNotNil: #form) = aForm
]

{ #category : #testing }
MorphicBackendForTest >> assertImage: actual equals: expected [ 
	
	(expected isNil and: [ actual isNil or: [actual width = 1 and: [ actual height =1 ]] ]) ifTrue: [ ^ self ].
	
	self assert: actual width = expected width.
	self assert: actual height = expected height.	
	self assert: actual bits = expected bits.		
]

{ #category : #asserting }
MorphicBackendForTest >> assertList: aFTTableMorph displayItems: aCollection [ 
	
	| tableContainer tableRowContents |
		
	self waitUntilUIRedrawed.
	
	tableContainer := aFTTableMorph submorphs first.
		
	tableRowContents := tableContainer submorphs collect: [ :row | 
		| cell |
		cell := row submorphs first.
		(cell submorphs detect: [:e | e isKindOf: StringMorph]) contents ].
	
	self assert: tableRowContents size = 3.
	
	self assert: tableRowContents first = aCollection first.
	self assert: tableRowContents second = aCollection second.
	self assert: tableRowContents third = aCollection third.
]

{ #category : #asserting }
MorphicBackendForTest >> assertListHeaderOf: aFTTableMorph equals: aCollection [ 

	| tableContainer tableRowMorph cells |
	
	self waitUntilUIRedrawed.
	
	tableContainer := aFTTableMorph submorphs first.
	tableRowMorph := tableContainer submorphs first.
	cells := tableRowMorph submorphs.
	
	self assert: (cells collect: [ :c | c submorphs first contents]) asArray = aCollection asArray
]

{ #category : #testing }
MorphicBackendForTest >> canChangeFonts [
	
	^ true
]

{ #category : #emulating }
MorphicBackendForTest >> clickFirstRowAndColumn: aFTTableMorph [ 

	self waitUntilUIRedrawed.
	
	aFTTableMorph mouseDown: (MouseEvent new
		setHand: HandMorph new;
		setPosition: aFTTableMorph submorphs first submorphs first bounds center;
		yourself).

]

{ #category : #asserting }
MorphicBackendForTest >> denyHasHeader: aFTTableMorph [ 

	| tableContainer |
	self waitUntilUIRedrawed.

	tableContainer := aFTTableMorph submorphs first.
	self assert: tableContainer submorphs size = 3
]

{ #category : #simulating }
MorphicBackendForTest >> doubleClickFirstRowAndColumn: aFTTableMorph [ 

	self waitUntilUIRedrawed.

	aFTTableMorph doubleClick: (MouseEvent new
		setPosition: aFTTableMorph submorphs first submorphs first bounds center;
		yourself).

]

{ #category : #running }
MorphicBackendForTest >> existsUIProcess [

	^ UIManager default uiProcess isNotNil
]

{ #category : #running }
MorphicBackendForTest >> initialize [

	super initialize.
	uiWaitingSemaphore := Semaphore new
]

{ #category : #running }
MorphicBackendForTest >> isRunningInUIProcess [

	^ UIManager default uiProcess == Processor activeProcess
]

{ #category : #opening }
MorphicBackendForTest >> openInstanceOf: aTest [ 

	aTest window ifNil: [ 
		aTest presenter application: app.
		aTest window: aTest presenter openWithSpec ].

	self waitUntilUIRedrawed.

]

{ #category : #running }
MorphicBackendForTest >> runTest: aBlock [
	app := SpecApplication new.
	app useBackend: #Morphic.
	aBlock value
]

{ #category : #running }
MorphicBackendForTest >> waitUntilUIRedrawed [

	"I wait until the UI has been redrawn. 
	I take care of selecting how to do it. 
	If I am in the CI I should defer a semaphore signal. 
	If I am running in the UI process I can directly execute a doOneCycle on the World.
	If I am in the CI the tests and the UI run in different process. So I should not do a #doOneCycle.
	If I do it, I am in a race condition!"
	(self isRunningInUIProcess) ifTrue: [ 
		self currentWorld doOneCycle.
		^ self.	
	]. 

	self currentWorld defer: [ uiWaitingSemaphore ifNotNil: #signal ].	
	uiWaitingSemaphore wait: 500 milliSecond
]
