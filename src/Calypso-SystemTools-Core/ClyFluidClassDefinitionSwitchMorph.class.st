"
I am a status bar item for class tools to switch between old class definition and new one which is based on slots
 
Internal Representation and Key Implementation Points.

    Instance Variables
	checkbox:		<CheckboxMorph>
"
Class {
	#name : #ClyFluidClassDefinitionSwitchMorph,
	#superclass : #ClyStatusBarItemMorph,
	#instVars : [
		'checkbox'
	],
	#category : #'Calypso-SystemTools-Core-Editors-Classes'
}

{ #category : #building }
ClyFluidClassDefinitionSwitchMorph >> build [
	
	checkbox := CheckboxMorph 
		on: self selected: #usesSlotBasedDefinition changeSelected: #toggle.
	checkbox
		getLabelSelector: #definitionTypeLabel;
		labelClickable: true;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		setBalloonText:	'To switch between old and new fluid class definition. The fluid definition will be default in the future'.
	self addMorph: checkbox
]

{ #category : #accessing }
ClyFluidClassDefinitionSwitchMorph >> definitionTypeLabel [
	^ 'Fluid'
]

{ #category : #configure }
ClyFluidClassDefinitionSwitchMorph >> toggle [

	ClassDefinitionPrinter toggleShowFluidClassDefinition.
	ownerTool update
]

{ #category : #configure }
ClyFluidClassDefinitionSwitchMorph >> usesSlotBasedDefinition [

	^ ClassDefinitionPrinter showFluidClassDefinition
]
