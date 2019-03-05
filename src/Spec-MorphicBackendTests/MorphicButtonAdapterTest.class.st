Class {
	#name : #MorphicButtonAdapterTest,
	#superclass : #MorphicAdapterTestCase,
	#category : #'Spec-MorphicBackendTests'
}

{ #category : #running }
MorphicButtonAdapterTest >> classToTest [

	^ ButtonPresenter
]

{ #category : #utils }
MorphicButtonAdapterTest >> emulateCenterClick [

	self emulateClick: MouseButtonEvent blueButton
]

{ #category : #utils }
MorphicButtonAdapterTest >> emulateClick: mouseButtonCode [

	| btn evt |
	"Force opening the spec instance here.
	The action should have been correctly configured before
	depending on the spec initialization strategy"
	self openInstance.
	
	btn := presenter adapter widget.
	evt := MouseButtonEvent new setType: nil position: btn center which: mouseButtonCode buttons: mouseButtonCode hand: nil stamp: nil.

	(btn handlesMouseDown: evt) ifTrue: [
		btn mouseDown: evt.
		btn mouseUp: evt].

]

{ #category : #utils }
MorphicButtonAdapterTest >> emulateLeftClick [

	self emulateClick: MouseButtonEvent redButton
]

{ #category : #utils }
MorphicButtonAdapterTest >> emulateRightClick [

	self emulateClick: MouseButtonEvent yellowButton
]

{ #category : #running }
MorphicButtonAdapterTest >> testChangingHelpAffectTheWidget [
	
	presenter help: 'ALabel'.
	self assert: self widget helpText equals: 'ALabel'
]

{ #category : #running }
MorphicButtonAdapterTest >> testChangingIconAffectTheWidget [
	
	presenter iconName: #pharo.
	self assert: self widget label icon form equals: (self iconNamed: #pharo)
]

{ #category : #running }
MorphicButtonAdapterTest >> testChangingLabelAffectTheWidget [
	
	presenter label: 'ALabel'.
	self assert: self widget label equals: 'ALabel'
]

{ #category : #running }
MorphicButtonAdapterTest >> testClickExecuteAction [
	| actionExecuted |	
	actionExecuted := false.
	presenter action: [ actionExecuted := true].

	self emulateLeftClick.
	self assert: actionExecuted
]

{ #category : #running }
MorphicButtonAdapterTest >> testFontAffectsTheWidget [

	presenter label: 'ALabel'.
	presenter font: StandardFonts codeFont.

	self assert: self widget label equals: 'ALabel'.
	self assert: self widget font equals: StandardFonts codeFont.

]

{ #category : #running }
MorphicButtonAdapterTest >> testNoClickDoesNotExecuteAction [
	| actionExecuted |
	actionExecuted := false.
	presenter action: [ actionExecuted := true].

	self deny: actionExecuted
]

{ #category : #running }
MorphicButtonAdapterTest >> testRightClickShouldNotExecuteAction [
	| actionExecuted |
	actionExecuted := false.
	presenter action: [ actionExecuted := true.].

	self emulateRightClick.
	self deny: actionExecuted
]
