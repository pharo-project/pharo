Class {
	#name : #SpTreeTablePresenterTest,
	#superclass : #SpTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpTreeTablePresenterTest >> classToTest [

	^ SpTreeTablePresenter
]

{ #category : #running }
SpTreeTablePresenterTest >> setUp [

	super setUp.
	presenter 
		roots: #(1 2 3);
		children: [ :aNumber | 
			aNumber < 100 
				ifTrue: [ ((aNumber * 10) to: (aNumber * 10) + 10) asArray ]
				ifFalse: [ #() ] ].
	
]

{ #category : #tests }
SpTreeTablePresenterTest >> testActivationOnDoubleClickShouldActivateOnDoubleClick [
	| activatedItem |
	presenter
		activateOnDoubleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].

	presenter doubleClickAtPath: #(1 2 1).

	self assert: activatedItem equals: 110
]

{ #category : #tests }
SpTreeTablePresenterTest >> testActivationOnDoubleClickShouldNotActivateOnClick [
	| activatedItem |
	presenter
		activateOnDoubleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].

	presenter clickAtPath: #(1).

	self assert: activatedItem isNil
]

{ #category : #tests }
SpTreeTablePresenterTest >> testActivationOnSingleClickShouldActivateOnClick [
	| activatedItem |
	presenter
		activateOnSingleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].

	presenter clickAtPath: #(1).

	self assert: activatedItem equals: 1
]

{ #category : #tests }
SpTreeTablePresenterTest >> testActivationOnSingleClickShouldNotActivateOnClickOutside [
	| activatedItem |
	presenter
		activateOnSingleClick;
		whenActivatedDo: [ :selection | activatedItem := selection selectedItem ].

	presenter clickAtPath: #(4).

	self assert: activatedItem isNil
]

{ #category : #tests }
SpTreeTablePresenterTest >> testContextMenu [
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
SpTreeTablePresenterTest >> testUnselectAll [

	presenter beMultipleSelection.
	presenter 
		selectPath: #(1);
		selectPath: #(2).
	self assert: presenter selection selectedPaths equals: #((1) (2)).
	presenter unselectAll.
	self assertEmpty: presenter selection selectedPaths.
	

]
