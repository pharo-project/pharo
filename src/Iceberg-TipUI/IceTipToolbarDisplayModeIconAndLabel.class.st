"
I implement display mode ""icons and labels""
"
Class {
	#name : #IceTipToolbarDisplayModeIconAndLabel,
	#superclass : #IceTipToolbarDisplayMode,
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #configuring }
IceTipToolbarDisplayModeIconAndLabel >> configureButton: aButton item: aToolbarItem [
	"ask for icon and label"
	aButton 
		on: aToolbarItem 
		getState: nil
		action: #execute
		label: #label 
		icon: #icon 
		menu: nil.
		
	aButton badgeSelector: #badge

]

{ #category : #accessing }
IceTipToolbarDisplayModeIconAndLabel >> extent [
	^ 45@40
]
