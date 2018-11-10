"
I am a non-editable, non-selectable text morph.
"
Class {
	#name : #IceTipLabelMorph,
	#superclass : #IceTipReadOnlyTextMorph,
	#category : #'Iceberg-TipUI-Spec-ReadOnlyForm'
}

{ #category : #initialization }
IceTipLabelMorph >> handlesMouseDown: evt [
	^ false
]

{ #category : #initialization }
IceTipLabelMorph >> mouseEnter: evt [
	"Handle a mouseEnter event, meaning the mouse just entered my bounds with no button pressed. The default response is to let my eventHandler, if any, handle it."

	"Overriding TextMorph method"
	self flag: #bad.

	^ self eventHandler ifNotNil:
		[self eventHandler mouseEnter: evt fromMorph: self].

]

{ #category : #initialization }
IceTipLabelMorph >> mouseLeave: evt [
	"Handle a mouseLeave event, meaning the mouse just left my bounds with no button pressed. The default response is to let my eventHandler, if any, handle it."

	"Overriding TextMorph method"
	self flag: #bad.
	
	^ self eventHandler ifNotNil:
		[self eventHandler mouseLeave: evt fromMorph: self].

]
