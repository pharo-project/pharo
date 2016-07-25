morphic ui
buttonRow: specArray
	| aRow |
	aRow := PanelMorph new.
	aRow layoutPolicy: TableLayout new; listDirection: #leftToRight.
	aRow hResizing: #spaceFill; vResizing: #spaceFill; rubberBandCells: true.
	aRow clipSubmorphs: true; borderWidth: 0.
	aRow layoutInset: 2@2; cellInset: 1.
	aRow wrapCentering: #center; cellPositioning: #leftCenter.
	specArray do:
		[:triplet | | aButton state |
			state := triplet at: 5 ifAbsent: [#buttonState].
			aButton := PluggableButtonMorph
				on: self
				getState: state
				action: #performButtonAction:enabled:.
			aButton
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				label: triplet first asString;
				getEnabledSelector: (triplet at: 4 ifAbsent: [#buttonEnabled]);
				arguments: (Array with: triplet second with: (triplet at: 4 ifAbsent: [#buttonEnabled])).
			aRow addMorphBack: aButton.
			aButton setBalloonText: triplet third].
	^ aRow