"
A CheckBoxExample is a simple example of how to use CheckBoxes.

self example
"
Class {
	#name : #CheckBoxExample,
	#superclass : #ComposablePresenter,
	#instVars : [
		'button1',
		'button2',
		'button3',
		'container',
		'morph1',
		'morph2',
		'morph3'
	],
	#category : 'Spec-Examples-Morphic'
}

{ #category : #specs }
CheckBoxExample class >> defaultSpec [

	^ { #ContainerPresenter.
			#add:.	{ self topSpec. #layout:. 	#(#SpecLayoutFrame
													bottomFraction: 0
													bottomOffset: 20) }.
			#add:.	{{#model . #container } . #layout: . #(#SpecLayoutFrame topOffset: 22) }
	   }
]

{ #category : #specs }
CheckBoxExample class >> defaultSpec2 [

	^ { #Panel.
			#changeTableLayout.
			#listDirection:.		#rightToLeft.
			#addMorph:. {#model.	#button1.}.
			#addMorph:. {#model.	#button2.}.
			#addMorph:. {#model.	#button3.}.
			#hResizing:.	#shrinkWrap.
			#vResizing:.	#shrinkWrap.	}
]

{ #category : #specs }
CheckBoxExample class >> defaultSpec3 [
	<spec: #default>
	^ SpecLayout composed
		newColumn: [ :column | 
			column
				newRow: [ :row | 
					row
						add: #button1 origin: 0 @ 0 corner: 0.3 @ 1;
						add: #button2 origin: 0.3 @ 0 corner: 0.6 @ 1;
						add: #button3 origin: 0.6 @ 0 corner: 1 @ 1 ]
					origin: 0 @ 0
					corner: 1 @ 0.3;
				add: #container origin: 0 @ 0.3 corner:1 @ 1 ];
		yourself
]

{ #category : #example }
CheckBoxExample class >> example [
	^ self new openWithSpec
]

{ #category : #specs }
CheckBoxExample class >> title [

	^ 'CheckBox Example'
]

{ #category : #specs }
CheckBoxExample class >> topSpec [

	^ { #ContainerPresenter.
			#add:. {{#model .	#button1 } . #layout:.  #(#SpecLayoutFrame
															rightFraction: 0.33
															bottomFraction: 0
															bottomOffset: 25)}.
			#add:.  {{#model .	#button2 } . #layout:. #(#SpecLayoutFrame
															leftFraction: 0.33
															rightFraction: 0.66
															bottomFraction: 0
															bottomOffset: 25)}.
			#add:.  {{#model .	#button3 } . #layout:. #(#SpecLayoutFrame
															leftFraction: 0.66
															bottomFraction: 0
															bottomOffset: 25)}}
]

{ #category : #accessing }
CheckBoxExample >> button1 [

	^ button1
]

{ #category : #accessing }
CheckBoxExample >> button2 [

	^ button2
]

{ #category : #accessing }
CheckBoxExample >> button3 [

	^ button3
]

{ #category : #accessing }
CheckBoxExample >> container [

	^ container  asSpecAdapter
]

{ #category : #initialization }
CheckBoxExample >> initialize [

	container := PanelMorph new.
	
	container
		changeTableLayout;
		listDirection: #bottomToLeft.
	
	self instantiateMorphs.
	super initialize.
]

{ #category : #initialization }
CheckBoxExample >> initializePresenter [

	self setActionsForButton1.
	self setActionsForButton2.
	self setActionsForButton3.	
]

{ #category : #initialization }
CheckBoxExample >> initializeWidgets [ 

	button1 := self newCheckBox.
	button2 := self newCheckBox.
	button3 := self newCheckBox.
	
	button1 label: 'Button 1'.
	button2 label: 'Button 2'.
	button3 label: 'Button 3'.
	self setFocus.
]

{ #category : #initialization }
CheckBoxExample >> instantiateMorphs [

	morph1 := Morph new
					color: Color red;
					width: 60;
					height: 20.
	morph2 := Morph new
					color: Color blue;
					width: 20;
					height: 60.
	morph3 := Morph new
					color: Color green;
					width: 50;
					height: 50.
]

{ #category : #initialization }
CheckBoxExample >> setActionsForButton1 [

	button1 whenActivatedDo: [ container addMorph: morph1 ].
	button1 whenDeactivatedDo: [ container removeMorph: morph1 ]
]

{ #category : #initialization }
CheckBoxExample >> setActionsForButton2 [

	button2 whenActivatedDo: [ container addMorph: morph2 ].
	button2 whenDeactivatedDo: [ container removeMorph: morph2 ]
]

{ #category : #initialization }
CheckBoxExample >> setActionsForButton3 [

	button3 whenActivatedDo: [ container addMorph: morph3 ].
	button3 whenDeactivatedDo: [ container removeMorph: morph3 ]
]

{ #category : #initialization }
CheckBoxExample >> setFocus [
	
	self focusOrder
		add: button1;
		add: button2;
		add: button3.
	
]

{ #category : #initialization }
CheckBoxExample >> setGroup [

	RadioButtonGroupPresenter new 
		addRadioButton: button1;
		addRadioButton: button2;
		addRadioButton: button3;
		default: button1.
	
]
