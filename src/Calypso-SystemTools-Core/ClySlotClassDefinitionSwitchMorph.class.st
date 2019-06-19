"
I am a status bar item for class tools to switch between old class definition and new one which is based on slots
 
Internal Representation and Key Implementation Points.

    Instance Variables
	checkbox:		<CheckboxMorph>
"
Class {
	#name : #ClySlotClassDefinitionSwitchMorph,
	#superclass : #ClyStatusBarItemMorph,
	#instVars : [
		'checkbox'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Classes'
}

{ #category : #building }
ClySlotClassDefinitionSwitchMorph >> build [
	
	checkbox := CheckboxMorph 
		on: self selected: #usesSlotBasedDefinition changeSelected: #toggle.
	checkbox
		getLabelSelector: #definitionTypeLabel;
		labelClickable: true;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		setBalloonText:	'You can switch between old and new class definition. New definition is based on slots'.
	self addMorph: checkbox
]

{ #category : #accessing }
ClySlotClassDefinitionSwitchMorph >> definitionTypeLabel [
	^ 'Slots'
]

{ #category : #operations }
ClySlotClassDefinitionSwitchMorph >> toggle [
	Slot showSlotClassDefinition: Slot showSlotClassDefinition not.
	
	ownerTool update
]

{ #category : #testing }
ClySlotClassDefinitionSwitchMorph >> usesSlotBasedDefinition [
	 
	^ Slot showSlotClassDefinition
]
