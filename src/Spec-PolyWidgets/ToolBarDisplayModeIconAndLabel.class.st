"
Display the toolbar with icons and label only
"
Class {
	#name : #ToolBarDisplayModeIconAndLabel,
	#superclass : #ToolBarDisplayMode,
	#category : #'Spec-PolyWidgets-ToolBar'
}

{ #category : #configuring }
ToolBarDisplayModeIconAndLabel >> configureButton: aButton item: aToolbarItem [
	"ask for icon and label"
	aButton getLabelSelector: #label.
	aButton getIconSelector: #icon.
	aButton badgeSelector: #badge.
]

{ #category : #accessing }
ToolBarDisplayModeIconAndLabel >> extent [
	^ 45@40
]
