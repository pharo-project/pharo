"
Description
--------------------

I am a simple demo to show how to use spacers in Spec layouts.

```

	SpecSpacerDemo example
	
```
"
Class {
	#name : #SpecSpacerDemo,
	#superclass : #ComposablePresenter,
	#instVars : [
		'input2',
		'input3',
		'input4',
		'button1',
		'button2',
		'button3',
		'button4',
		'input1'
	],
	#category : #'Spec-Examples-Demo-Layouts'
}

{ #category : #specs }
SpecSpacerDemo class >> defaultSpec [
	^ SpecLayout composed
		newColumn: [ :c | 
			c
				newRow: #input1 height: self inputTextHeight;
				addSpacer;
				newRow: #input2 height: self inputTextHeight;
				newRow: #input3 height: self inputTextHeight;
				addSpacer;
				addSpacer;
				newRow: #input4 height: self inputTextHeight;
				addSpacer;
				newRow: [ :r | 
					r
						add: #button1;
						addSpacer;
						add: #button2;
						"addSpacer;"
						add: #button3;
						"addSpacer;"
						add: #button4 ]
					height: self toolbarHeight ];
		yourself
]

{ #category : #specs }
SpecSpacerDemo class >> example [
	^ self new openWithSpec
]

{ #category : #specs }
SpecSpacerDemo class >> title [
	^ 'Spacer demo'
]

{ #category : #accessing }
SpecSpacerDemo >> button1 [
	^ button1
]

{ #category : #accessing }
SpecSpacerDemo >> button2 [
	^ button2
]

{ #category : #accessing }
SpecSpacerDemo >> button3 [
	^ button3
]

{ #category : #accessing }
SpecSpacerDemo >> button4 [
	^ button4
]

{ #category : #specs }
SpecSpacerDemo >> extent [
	^ 700@500
]

{ #category : #initialization }
SpecSpacerDemo >> initializeWidgets [
	input1 := self newTextInput.
	input2 := self newTextInput.
	input3 := self newTextInput.
	input4 := self newTextInput.
	
	button1 := self newButton.
	button2 := self newButton.
	button3 := self newButton.
	button4 := self newButton.
	
	input1 placeholder: 'Input 1'.
	input2 placeholder: 'Input 2'.
	input3 placeholder: 'Input 3'.
	input4 placeholder: 'Input 4'.
	
	button1 label: 'Button 1'.
	button2 label: 'Button 2'.
	button3 label: 'Button 3'.
	button4 label: 'Button 4'
]

{ #category : #accessing }
SpecSpacerDemo >> input1 [
	^ input1
]

{ #category : #accessing }
SpecSpacerDemo >> input2 [
	^ input2
]

{ #category : #accessing }
SpecSpacerDemo >> input3 [
	^ input3
]

{ #category : #accessing }
SpecSpacerDemo >> input4 [
	^ input4
]
