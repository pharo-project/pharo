Class {
	#name : #ButtonAdapterTest,
	#superclass : #AbstractWidgetAdapterTestCase,
	#category : #'Spec-BackendTests'
}

{ #category : #running }
ButtonAdapterTest >> classToTest [

	^ ButtonPresenter
]

{ #category : #running }
ButtonAdapterTest >> testChangingIconAffectTheWidget [
	
	presenter iconName: #pharo.
	
	self assertIcon: self adapter icon equals: (self iconNamed: #pharo)
]

{ #category : #running }
ButtonAdapterTest >> testChangingLabelAffectTheWidget [
	
	presenter label: 'ALabel'.
	self assert: self widget label equals: 'ALabel'
]

{ #category : #running }
ButtonAdapterTest >> testClickExecuteAction [
	| actionExecuted |	

	actionExecuted := false.
	presenter action: [ actionExecuted := true].

	self emulateLeftClick.
	self assert: actionExecuted
]

{ #category : #running }
ButtonAdapterTest >> testFontAffectsTheWidget [

	backendForTest canChangeFonts ifFalse: [ self skip ].

	presenter label: 'ALabel'.
	presenter font: StandardFonts codeFont.

	self assert: self widget label equals: 'ALabel'.
	self assert: self widget font equals: StandardFonts codeFont.

]

{ #category : #running }
ButtonAdapterTest >> testNoClickDoesNotExecuteAction [
	| actionExecuted |
	actionExecuted := false.
	presenter action: [ actionExecuted := true].

	self deny: actionExecuted
]

{ #category : #running }
ButtonAdapterTest >> testRightClickShouldNotExecuteAction [
	| actionExecuted |

	actionExecuted := false.
	presenter action: [ actionExecuted := true.].

	self emulateRightClick.
	self deny: actionExecuted
]

{ #category : #running }
ButtonAdapterTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpecMockMenuPresenter new).
	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]

{ #category : #running }
ButtonAdapterTest >> testWhenActivatedDo [

	| actionExecuted |	
	actionExecuted := false.
	presenter whenActivatedDo: [ actionExecuted := true].

	self emulateLeftClick.
	self assert: actionExecuted
]
