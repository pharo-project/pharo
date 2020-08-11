"
Display the toolbar with labels only
"
Class {
	#name : #SpToolbarDisplayModeLabel,
	#superclass : #SpToolbarDisplayMode,
	#category : #'Spec2-Core-Widgets'
}

{ #category : #configuring }
SpToolbarDisplayModeLabel >> configureButton: aButton item: aToolbarItem [
	"ask for label (no icon)"
	aButton getLabelSelector: #label.
	aButton badgeSelector: #badge.
]

{ #category : #accessing }
SpToolbarDisplayModeLabel >> extent [
	^ 45@25
]

{ #category : #accessing }
SpToolbarDisplayModeLabel >> styleName [

	^ 'toolBar.label'
]
