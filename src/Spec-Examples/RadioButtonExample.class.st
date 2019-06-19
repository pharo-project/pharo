"
I am a demo to show how to use radio buttons with Spec.

self example
"
Class {
	#name : #RadioButtonExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'button1',
		'button2',
		'button3',
		'label'
	],
	#category : #'Spec-Examples-Demo-Checkboxes'
}

{ #category : #specs }
RadioButtonExample class >> defaultSpec [
	^ SpecBoxLayout newVertical
		add:
			(SpecBoxLayout newHorizontal
				add: #button1;
				add: #button2;
				add: #button3;
				yourself)
			withConstraints: [ :c | c height: self toolbarHeight ];
		add: #label withConstraints: [ :c | c height: self labelHeight ];
		yourself
]

{ #category : #example }
RadioButtonExample class >> example [
	<sampleInstance>
	^ self new openWithSpec
]

{ #category : #specs }
RadioButtonExample class >> title [

	^ 'Radio Button Example'
]

{ #category : #initialization }
RadioButtonExample >> initializePresenter [
	button1 whenActivatedDo: [ self updateLabel ].
	button1 whenDeactivatedDo: [ self updateLabel ].

	button2 whenChangedDo: [ self updateLabel ].

	button3 whenChangedDo: [ self updateLabel ]
]

{ #category : #initialization }
RadioButtonExample >> initializeWidgets [
	button1 := self newRadioButton.
	button2 := self newRadioButton.
	button3 := self newRadioButton.
	label := self newLabel.

	button1 associatedRadioButtons: {button2 . button3}.

	button1 label: 'Button 1'.
	button2 label: 'Button 2'.
	button3 label: 'Button 3'.

	self updateLabel.
	self setFocus
]

{ #category : #initialization }
RadioButtonExample >> setFocus [
	self focusOrder
		add: button1;
		add: button2;
		add: button3
]

{ #category : #updating }
RadioButtonExample >> updateLabel [
	label
		label:
			(String
				streamContents: [ :s | 
					{button1 . button2 . button3}
						do: [ :button | 
							s
								<< button label;
								<< ' : ';
								<< (button state ifTrue: [ 'V' ] ifFalse: [ 'X' ]) ]
						separatedBy: [ s << ' - ' ] ])
]
