"
Specialized border for TabGroup. Does not draw border beneath the selectd tab and only draws on top.
"
Class {
	#name : #GLMTabPanelBorder,
	#superclass : #TabPanelBorder,
	#category : 'Glamour-Morphic-Theme'
}

{ #category : #drawing }
GLMTabPanelBorder >> frameRectangle: aRectangle on: aCanvas [
	"Draw the border taking the currently selected tab into account.
	Only works for top-positioned tabs for the moment."
	
	|w h r tab|
	w := self width.
	w isPoint ifTrue: [h := w y. w := w x] ifFalse:[h := w].
	tab := self selectedTab.
	tab ifNil: [	
		"r := aRectangle topLeft + (w@0) corner: aRectangle topRight - (w@h negated).
		aCanvas fillRectangle: r color: self color."
		^self]. "top"
	r := aRectangle topLeft corner: tab bounds left @(aRectangle top + h).
	aCanvas fillRectangle: r color: self color. 
	r :=  tab bounds right @ aRectangle top corner: aRectangle topRight - (w@h negated).
	aCanvas fillRectangle: r color: self color.
	
	r := aRectangle topLeft corner: aRectangle left + w @ (aRectangle bottom).
	aCanvas fillRectangle: r color: tab paneColor.
	r := aRectangle topRight corner: aRectangle right + w @ (aRectangle bottom).
	aCanvas fillRectangle: r color: tab paneColor.
]
