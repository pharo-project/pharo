"
I implement display mode ""just icons""
"
Class {
	#name : #IceTipToolbarDisplayModeIcon,
	#superclass : #IceTipToolbarDisplayMode,
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #configuring }
IceTipToolbarDisplayModeIcon >> configureButton: aButton item: aToolbarItem [
	"ask for icon (no label)"
	aButton 
		on: aToolbarItem 
		getState: nil
		action: #execute
		label: nil
		icon: #icon 
		menu: nil.
		
	aButton badgeSelector: #badge.
]

{ #category : #accessing }
IceTipToolbarDisplayModeIcon >> extent [
	^ 25@25
]
