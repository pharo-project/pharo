"
I implement display mode ""just labels""
"
Class {
	#name : #IceTipToolbarDisplayModeLabel,
	#superclass : #IceTipToolbarDisplayMode,
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #configuring }
IceTipToolbarDisplayModeLabel >> configureButton: aButton item: aToolbarItem [
	"ask for label (no icon)"
	aButton 
		on: aToolbarItem 
		getState: nil
		action: #execute
		label: #label 
		icon: nil		
		menu: nil

]

{ #category : #accessing }
IceTipToolbarDisplayModeLabel >> extent [
	^ 45@25
]
