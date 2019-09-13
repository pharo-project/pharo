"
Display the toolbar with icons only
"
Class {
	#name : #SpToolBarDisplayModeIcon,
	#superclass : #SpToolBarDisplayMode,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #configuring }
SpToolBarDisplayModeIcon >> configureButton: aButton item: aToolbarItem [
	"ask for icon (no label)"
	aButton getIconSelector: #icon.
	aButton badgeSelector: #badge.
]

{ #category : #accessing }
SpToolBarDisplayModeIcon >> extent [

	^ 30@30
]

{ #category : #accessing }
SpToolBarDisplayModeIcon >> styleName [ 

	^ 'toolBar.icons'
]
