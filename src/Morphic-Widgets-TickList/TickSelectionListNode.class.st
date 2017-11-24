"
A TickSelectionListNode is node for the tree used to render the tick list

selected
	- xxxxx

"
Class {
	#name : #TickSelectionListNode,
	#superclass : #MorphTreeNodeModel,
	#category : #'Morphic-Widgets-TickList'
}

{ #category : #private }
TickSelectionListNode >> name [

	^ self item itemName asString
]

{ #category : #morph }
TickSelectionListNode >> nameMorph [

	^ LabelMorph new contents: self name; enabled: false; hResizing: #spaceFill;yourself
]

{ #category : #event }
TickSelectionListNode >> pressed: anIndex [

	self model pressed: anIndex
]

{ #category : #accessing }
TickSelectionListNode >> selected [

	^ self item selected
]

{ #category : #accessing }
TickSelectionListNode >> selected: aBoolean [

	self item selected: aBoolean
]

{ #category : #morph }
TickSelectionListNode >> selectedMorph [
	^ (TickSelectionCheckboxMorph basicNew index: (self model rootItems indexOf: self item);
		initialize;	
		model: self item;
		getStateSelector: #selected;
		setStateSelector: #selected:;
		updateSelection;
		getEnabledSelector: nil;
		font: StandardFonts defaultFont;
		label: '';
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		setBalloonText: nil) color: Color transparent; yourself
]

{ #category : #private }
TickSelectionListNode >> toogleSelectionValue [
	
	self item toogleSelectionValue
]
