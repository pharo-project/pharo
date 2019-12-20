"
Display the toolbar with labels only
"
Class {
	#name : #SpToolBarDisplayModeLabel,
	#superclass : #SpToolBarDisplayMode,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #configuring }
SpToolBarDisplayModeLabel >> configureButton: aButton item: aToolbarItem [
	"ask for label (no icon)"
	aButton getLabelSelector: #label.
	aButton badgeSelector: #badge.
]

{ #category : #accessing }
SpToolBarDisplayModeLabel >> extent [
	^ 45@25
]

{ #category : #accessing }
SpToolBarDisplayModeLabel >> styleName [

	^ 'toolBar.label'
]
