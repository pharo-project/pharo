Class {
	#name : #SpTreePresenterTest,
	#superclass : #SpSpecTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpTreePresenterTest >> classToTest [

	^ SpTreePresenter
]

{ #category : #running }
SpTreePresenterTest >> setUp [

	super setUp.
	presenter 
		roots: #(1 2 3);
		children: [ :aNumber | 
			aNumber < 100 
				ifTrue: [ ((aNumber * 10) to: (aNumber * 10) + 10) asArray ]
				ifFalse: [ #() ] ].
	
]

{ #category : #tests }
SpTreePresenterTest >> testActivationOnDoubleClickShouldActivateOnDoubleClick [
	| activatedItem |
	presenter
		activateOnDoubleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].

	presenter doubleClickAtPath: #(1 2 1).

	self assert: activatedItem equals: 110
]

{ #category : #tests }
SpTreePresenterTest >> testActivationOnDoubleClickShouldNotActivateOnClick [
	| activatedItem |
	presenter
		activateOnDoubleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].

	presenter clickAtPath: #(1).

	self assert: activatedItem isNil
]

{ #category : #tests }
SpTreePresenterTest >> testActivationOnSingleClickShouldActivateOnClick [
	| activatedItem |
	presenter
		activateOnSingleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].

	presenter clickAtPath: #(1).

	self assert: activatedItem equals: 1
]

{ #category : #tests }
SpTreePresenterTest >> testActivationOnSingleClickShouldNotActivateOnClickOutside [
	| activatedItem |
	presenter
		activateOnSingleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].

	presenter clickAtPath: #(4).

	self assert: activatedItem isNil
]

{ #category : #tests }
SpTreePresenterTest >> testContextMenu [
	| menu changed |
	self assert: presenter contextMenu isNil.
	menu := SpMenuPresenter new.
	changed := false.
	presenter whenMenuChangedDo: [ 
		changed := true. ].
	presenter contextMenu: menu.
	self assert: presenter contextMenu equals: menu.
	self assert: changed
]

{ #category : #tests }
SpTreePresenterTest >> testUnselectAll [

	presenter beMultipleSelection.
	presenter 
		selectPath: #(1);
		selectPath: #(2).
	self assert: presenter selection selectedPaths equals: #((1) (2)).
	presenter unselectAll.
	self assertEmpty: presenter selection selectedPaths.
	

]
