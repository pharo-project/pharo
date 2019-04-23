"
Description
--------------------

I am a presenter to show how to use modals with spec.
"
Class {
	#name : #SpecDemoModalPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'button1',
		'button2'
	],
	#category : #'Spec-Examples-Demo-Modals'
}

{ #category : #specs }
SpecDemoModalPresenter class >> defaultSpec [
	^ SpecLayout composed
		newRow: [ :r | 
			r
				newColumn: #button1;
				newColumn: #button2 ]
		height: self buttonHeight
]

{ #category : #'instance creation' }
SpecDemoModalPresenter class >> open [
	<script>
	self new openWithSpec
]

{ #category : #specs }
SpecDemoModalPresenter class >> title [
	^ 'Modals demo'
]

{ #category : #accessing }
SpecDemoModalPresenter >> button1 [
	^ button1
]

{ #category : #accessing }
SpecDemoModalPresenter >> button2 [
	^ button2
]

{ #category : #initialization }
SpecDemoModalPresenter >> initializeWidgets [
	button1 := self newButton.
	button2 := self newButton.
	
	button1
		label: 'Open 1';
		action: [ SpecDemoModal1Presenter new openModalWithSpec ].
	button2 label: 'Open 2';
		action: [ SpecDemoModal2Presenter new openModalWithSpec ]
]
