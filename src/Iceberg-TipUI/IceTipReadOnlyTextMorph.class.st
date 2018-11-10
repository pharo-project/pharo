"
I'm a read-only text morph. 
I catch key events and forbid them to happen. 

This is useful to use in components where I do not want to allow edition, but I want to allow coping anyway.
"
Class {
	#name : #IceTipReadOnlyTextMorph,
	#superclass : #TextMorph,
	#category : #'Iceberg-TipUI-Spec-ReadOnlyForm'
}

{ #category : #'event handling' }
IceTipReadOnlyTextMorph >> handleKeystroke: anEvent [
	anEvent wasHandled
		ifTrue: [^ self].
	self allowsKeymapping
		ifTrue: [ self dispatchKeystrokeForEvent: anEvent ]
]

{ #category : #initialization }
IceTipReadOnlyTextMorph >> initialize [
	super initialize.
	self 
		bindKeyCombination: PharoShortcuts current copySelectionShortcut 
		toAction: [ :target | target editor copySelection ].
	self 
		bindKeyCombination: PharoShortcuts current selectAllShortcut
		toAction: [ :target | target handleEdit: [ target selectAll ] ]
]
