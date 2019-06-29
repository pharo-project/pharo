Class {
	#name : #SpMorphicBackendForTest,
	#superclass : #SpAbstractBackendForTest,
	#instVars : [
		'uiWaitingSemaphore',
		'app'
	],
	#category : #'Spec2-Backend-Tests'
}

{ #category : #asserting }
SpMorphicBackendForTest >> assertHasHeader: aFTTableMorph [
	self waitUntilUIRedrawed.

	self assert: (aFTTableMorph instVarNamed: #showColumnHeaders)
]

{ #category : #asserting }
SpMorphicBackendForTest >> assertIcon: anAlphaImageMorph equals: aForm [ 
	
	self assert: (anAlphaImageMorph ifNotNil: #form) = aForm
]

{ #category : #testing }
SpMorphicBackendForTest >> assertImage: actual equals: expected [ 
	
	(expected isNil and: [ actual isNil or: [actual width = 1 and: [ actual height =1 ]] ]) ifTrue: [ ^ self ].
	
	self assert: actual width = expected width.
	self assert: actual height = expected height.	
	self assert: actual bits = expected bits.		
]

{ #category : #asserting }
SpMorphicBackendForTest >> assertList: aFTTableMorph displayItems: aCollection [ 
	
	| tableContainer tableRowContents |
	
	self waitUntilUIRedrawed.
	
	tableContainer := aFTTableMorph submorphs first.
	tableRowContents := tableContainer submorphs collect: [ :row | 
		| cell |
		cell := row submorphs first.
		(cell submorphs detect: [:e | e isKindOf: StringMorph]) contents ].
	
	self assert: tableRowContents first = aCollection first.
	self assert: tableRowContents second = aCollection second.
	self assert: tableRowContents third = aCollection third.
]

{ #category : #asserting }
SpMorphicBackendForTest >> assertListHeaderOf: aFTTableMorph equals: aCollection [ 

	| tableContainer tableRowMorph cells |
	
	self waitUntilUIRedrawed.
	
	tableContainer := aFTTableMorph submorphs first.
	tableRowMorph := tableContainer submorphs first.
	cells := tableRowMorph submorphs.
	
	self assert: (cells collect: [ :c | c submorphs first contents]) asArray = aCollection asArray
]

{ #category : #testing }
SpMorphicBackendForTest >> canChangeFonts [
	
	^ true
]

{ #category : #emulating }
SpMorphicBackendForTest >> clickFirstRowAndColumn: aFTTableMorph [ 

	self waitUntilUIRedrawed.
	
	aFTTableMorph mouseDown: (MouseEvent new
		setHand: HandMorph new;
		setPosition: aFTTableMorph submorphs first submorphs first bounds center;
		yourself).

]

{ #category : #asserting }
SpMorphicBackendForTest >> denyHasHeader: aFTTableMorph [ 

	| tableContainer |
	self waitUntilUIRedrawed.

	tableContainer := aFTTableMorph submorphs first.
	self assert: tableContainer submorphs size = 3
]

{ #category : #simulating }
SpMorphicBackendForTest >> doubleClickFirstRowAndColumn: aFTTableMorph [ 

	self waitUntilUIRedrawed.

	aFTTableMorph doubleClick: (MouseEvent new
		setPosition: aFTTableMorph submorphs first submorphs first bounds center;
		yourself).

]

{ #category : #running }
SpMorphicBackendForTest >> initialize [

	super initialize.
	uiWaitingSemaphore := Semaphore new
]

{ #category : #running }
SpMorphicBackendForTest >> isRunningInUIProcess [

	^ UIManager default uiProcess == Processor activeProcess
]

{ #category : #opening }
SpMorphicBackendForTest >> openInstanceOf: aTest [ 

	aTest window ifNil: [ 
		aTest presenter application: app.
		aTest window: aTest presenter openWithSpec ].

	self waitUntilUIRedrawed.

]

{ #category : #running }
SpMorphicBackendForTest >> runTest: aBlock [
	app := SpApplication new.
	app useBackend: #Morphic.
	aBlock value
]

{ #category : #running }
SpMorphicBackendForTest >> waitUntilUIRedrawed [

	"I wait until the UI has been redrawn. 
	I take care of selecting how to do it. 
	If I am in the CI I should defer a semaphore signal. 
	If I am running in the UI process I can directly execute a doOneCycle on the World.
	If I am in the CI the tests and the UI run in different process. So I should not do a #doOneCycle.
	If I do it, I am in a race condition!"
	self isRunningInUIProcess ifTrue: [ 
		self currentWorld doOneCycle.
		^ self.	
	]. 

	self currentWorld defer: [ uiWaitingSemaphore ifNotNil: #signal ].	
	uiWaitingSemaphore wait: 500 milliSecond
]
