as yet unclassified
openModal: aSystemWindow
	"Open the given window an available position without modality.
	Answer the system window."
	
	|baseArea areas searching foundRect|
	aSystemWindow extent: aSystemWindow initialExtent.
	areas := World submorphs
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