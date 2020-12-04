"
Display the toolbar with icons only
"
Class {
	#name : #SpToolbarDisplayModeIcon,
	#superclass : #SpToolbarDisplayMode,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #configuring }
SpToolbarDisplayModeIcon >> configureButton: aButton item: aToolbarItem [
	"ask for icon (no label)"
	aButton getIconSelector: #icon.
	aButton badgeSelector: #badge.
]

{ #category : #accessing }
SpToolbarDisplayModeIcon >> extent [

	^ 30@30
]

{ #category : #accessing }
SpToolbarDisplayModeIcon >> styleName [ 

	^ 'toolBar.icons'
]
