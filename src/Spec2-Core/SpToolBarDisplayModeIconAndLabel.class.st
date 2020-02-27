"
Display the toolbar with icons and label only
"
Class {
	#name : #SpToolBarDisplayModeIconAndLabel,
	#superclass : #SpToolBarDisplayMode,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #configuring }
SpToolBarDisplayModeIconAndLabel >> configureButton: aButton item: aToolbarItem [
	"ask for icon and label"
	aButton getLabelSelector: #label.
	aButton getIconSelector: #icon.
	aButton badgeSelector: #badge.
]

{ #category : #accessing }
SpToolBarDisplayModeIconAndLabel >> extent [
	^ 45@40
]

{ #category : #accessing }
SpToolBarDisplayModeIconAndLabel >> styleName [ 

	^ 'toolBar.iconsAndLabel'
]
