"
Simple themed separator morph.
"
Class {
	#name : #SeparatorMorph,
	#superclass : #PanelMorph,
	#category : #'Polymorph-Widgets'
}

{ #category : #accessing }
SeparatorMorph >> adoptPaneColor: paneColor [
	"Change our fill too."
	
	super adoptPaneColor: paneColor.
	paneColor ifNil: [^self].
	self fillStyle: (self theme separatorFillStyleFor: self)
]
