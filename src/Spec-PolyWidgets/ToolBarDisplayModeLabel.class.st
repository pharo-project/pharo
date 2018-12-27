"
Display the toolbar with labels only
"
Class {
	#name : #ToolBarDisplayModeLabel,
	#superclass : #ToolBarDisplayMode,
	#category : #'Spec-PolyWidgets-ToolBar'
}

{ #category : #configuring }
ToolBarDisplayModeLabel >> configureButton: aButton item: aToolbarItem [
	"ask for label (no icon)"
	aButton getLabelSelector: #label.
	aButton badgeSelector: #badge.
]

{ #category : #accessing }
ToolBarDisplayModeLabel >> extent [
	^ 45@25
]
