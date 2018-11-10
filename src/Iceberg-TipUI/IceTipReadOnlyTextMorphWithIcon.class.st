Class {
	#name : #IceTipReadOnlyTextMorphWithIcon,
	#superclass : #ComposablePresenter,
	#instVars : [
		'iconWidget',
		'textWidget'
	],
	#category : #'Iceberg-TipUI-Spec-ReadOnlyForm'
}

{ #category : #specs }
IceTipReadOnlyTextMorphWithIcon class >> defaultSpec [
	<spec: #default>

	^ SpecLayout composed
		newRow: [ :row | 
				row newColumn: #iconWidget width: 40.
				row newColumn: #textWidget].	

]

{ #category : #specs }
IceTipReadOnlyTextMorphWithIcon class >> example [
	<example>
	
	^ self new
		iconName: #warning;
		text: String loremIpsum;
		openWithSpec 
]

{ #category : #accessing }
IceTipReadOnlyTextMorphWithIcon >> icon: anIcon [
	
	iconWidget widget 
		removeAllMorphs;
		addMorphBack: (anIcon asMorph)
]

{ #category : #accessing }
IceTipReadOnlyTextMorphWithIcon >> iconName: aString [ 
	self icon: ((self iconNamed: aString) scaledToSize: 32@32)
]

{ #category : #accessing }
IceTipReadOnlyTextMorphWithIcon >> iconWidget [
	^ iconWidget
]

{ #category : #initialization }
IceTipReadOnlyTextMorphWithIcon >> initializeWidgets [
	
	iconWidget := self newIcePanel.
	textWidget := self newIceReadOnlyText 
]

{ #category : #accessing }
IceTipReadOnlyTextMorphWithIcon >> text: aString [ 
	textWidget widget contentsWrapped: aString
]

{ #category : #accessing }
IceTipReadOnlyTextMorphWithIcon >> textWidget [
	^ textWidget
]
