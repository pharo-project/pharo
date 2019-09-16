Class {
	#name : #SpVersatileDialogPresenterTest,
	#superclass : #SpSmokeTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpVersatileDialogPresenterTest >> classToTest [
	^ SpVersatileDialogPresenter
]

{ #category : #tests }
SpVersatileDialogPresenterTest >> testListBox [
	| dialog app listPresenter |
	app := SpMockApplication new.

	dialog := SpVersatileDialogPresenter newApplication: app.
	listPresenter := dialog newList.
	listPresenter items: #(one two three).
	dialog contentArea: listPresenter.
	dialog
		addButton: #ok
		text: 'OK'
		value: #ok
		condition: [ listPresenter selection isEmpty not ].
	dialog addButton: #cancel text: 'Cancel' value: nil.
	dialog mainIcon: (self iconNamed: #question).

	dialog openModalWithSpec.
	dialog withWindowDo: [ :w | w title: 'Confirmation' ].

	dialog contentArea selection selectIndex: 2.

	(dialog buttons at: #ok) click.

	self assert: dialog result equals: #ok.
	self assert: dialog contentArea selection selectedItem equals: #two
]

{ #category : #tests }
SpVersatileDialogPresenterTest >> testResult [ 

	| dialog app |
	
	app := SpMockApplication new.
	
	dialog := SpVersatileDialogPresenter newApplication: app.
	dialog mainMessage: 'Save content' asText allBold.
	dialog addButton: #save text: 'Save' value: true.
	dialog addButton: #cancel text: 'Cancel' value: false.
	dialog mainIcon: (self iconNamed: #question).
	dialog moreOptionsArea: (dialog newCheckBox label: 'Save header').

	dialog openModalWithSpec. 
	dialog withWindowDo: [ :w | w title: 'Confirmation' ].
	(dialog buttons at: #save) performAction.
	dialog moreOptionsArea click.

	self assert: dialog result.	
	self assert: dialog moreOptionsArea state.
]
