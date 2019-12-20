Class {
	#name : #SpButtonAdapterTest,
	#superclass : #SpAbstractWidgetAdapterTest,
	#category : #'Spec2-Backend-Tests'
}

{ #category : #running }
SpButtonAdapterTest >> classToTest [

	^ SpButtonPresenter
]

{ #category : #running }
SpButtonAdapterTest >> testChangingIconAffectTheWidget [
	
	presenter iconName: #pharo.
	
	self assertIcon: self adapter icon equals: (self iconNamed: #pharo)
]

{ #category : #running }
SpButtonAdapterTest >> testChangingLabelAffectTheWidget [
	
	presenter label: 'ALabel'.
	self assert: self widget label equals: 'ALabel'
]

{ #category : #running }
SpButtonAdapterTest >> testClickExecuteAction [
	| actionExecuted |	

	actionExecuted := false.
	presenter action: [ actionExecuted := true].

	self emulateLeftClick.
	self assert: actionExecuted
]

{ #category : #running }
SpButtonAdapterTest >> testFontAffectsTheWidget [

	backendForTest canChangeFonts ifFalse: [ self skip ].

	presenter label: 'ALabel'.
	presenter font: StandardFonts codeFont.

	self assert: self widget label equals: 'ALabel'.
	self assert: self widget font equals: StandardFonts codeFont.

]

{ #category : #running }
SpButtonAdapterTest >> testNoClickDoesNotExecuteAction [
	| actionExecuted |
	actionExecuted := false.
	presenter action: [ actionExecuted := true].

	self deny: actionExecuted
]

{ #category : #running }
SpButtonAdapterTest >> testRightClickShouldNotExecuteAction [
	| actionExecuted |

	actionExecuted := false.
	presenter action: [ actionExecuted := true.].

	self emulateRightClick.
	self deny: actionExecuted
]

{ #category : #running }
SpButtonAdapterTest >> testRightClickShowsMenu [
	| menu |

	presenter contextMenu: (menu := SpMockMenuPresenter new).
	self deny: menu shown.
	self emulateRightClick.
	self assert: menu shown
]

{ #category : #running }
SpButtonAdapterTest >> testWhenActivatedDo [

	| actionExecuted |	
	actionExecuted := false.
	presenter whenActivatedDo: [ actionExecuted := true].

	self emulateLeftClick.
	self assert: actionExecuted
]
