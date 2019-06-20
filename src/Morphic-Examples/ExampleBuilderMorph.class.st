"
Morph with an inset border by default and theme access. Overrides openModal: to allow multiple free example dialogs to be presented.
"
Class {
	#name : #ExampleBuilderMorph,
	#superclass : #MorphicModel,
	#traits : 'TEasilyThemed',
	#classTraits : 'TEasilyThemed classTrait',
	#category : #'Morphic-Examples-Widgets'
}

{ #category : #services }
ExampleBuilderMorph >> chooseColor: aColor title: label [
	"Answer the user choice of a colour."
	
	^ self theme 
		chooseColorIn: self modalMorph
		title: (label ifNil: ['Choose Color' translated])
		color: aColor
]

{ #category : #'as yet unclassified' }
ExampleBuilderMorph >> openModal: aSystemWindow [
	"Open the given window an available position without modality.
	Answer the system window."
	
	|baseArea areas searching foundRect|
	aSystemWindow extent: aSystemWindow initialExtent.
	areas := self currentWorld submorphs
		select: [:m | m isKindOf: DialogWindow]
		thenCollect: [:m | m bounds expandBy: 8].
	baseArea := RealEstateAgent maximumUsableArea insetBy: 8.
	searching := true.
	baseArea allAreasOutsideList: areas do: [:rect |
		searching ifTrue: [
			aSystemWindow extent <= (rect insetBy: 8) extent
				ifTrue: [foundRect := rect.
						searching := false]]].
	searching ifTrue: [foundRect := baseArea].
	aSystemWindow setWindowColor: self theme windowColor.
	aSystemWindow position: foundRect topLeft + 8.
	aSystemWindow openAsIs.
	^aSystemWindow
]
