"
A CheckBoxExample is a simple example of how to use CheckBoxes.

self example
"
Class {
	#name : #SpCheckBoxExample,
	#superclass : #SpPresenter,
	#instVars : [
		'button1',
		'button2',
		'button3',
		'label'
	],
	#category : #'Spec2-Examples-Demo-Checkboxes'
}

{ #category : #specs }
SpCheckBoxExample class >> defaultSpec [
	^ SpBoxLayout newVertical
		add:
			(SpBoxLayout newHorizontal
				add: #button1;
				add: #button2;
				add: #button3;
				yourself)
			withConstraints: [ :c | c height: self toolbarHeight ];
		add: #label withConstraints: [ :c | c height: self labelHeight ];
		yourself
]

{ #category : #example }
SpCheckBoxExample class >> example [
	<sampleInstance>
	^ self new openWithSpec
]

{ #category : #specs }
SpCheckBoxExample class >> title [

	^ 'CheckBox Example'
]

{ #category : #initialization }
SpCheckBoxExample >> connectPresenters [
	button1 whenActivatedDo: [ self updateLabel ].
	button1 whenDeactivatedDo: [ self updateLabel ].

	button2 whenChangedDo: [ self updateLabel ].

	button3 whenChangedDo: [ self updateLabel ]
]

{ #category : #initialization }
SpCheckBoxExample >> initializePresenters [ 

	button1 := self newCheckBox.
	button2 := self newCheckBox.
	button3 := self newCheckBox.
	label := self newLabel.
	
	button1 label: 'Button 1'.
	button2 label: 'Button 2'.
	button3 label: 'Button 3'.
	
	self updateLabel.
	self setFocus.
]

{ #category : #initialization }
SpCheckBoxExample >> setFocus [
	self focusOrder
		add: button1;
		add: button2;
		add: button3
]

{ #category : #updating }
SpCheckBoxExample >> updateLabel [
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
