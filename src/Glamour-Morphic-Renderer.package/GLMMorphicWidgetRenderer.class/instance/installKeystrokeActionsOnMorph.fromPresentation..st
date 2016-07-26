rendering
installKeystrokeActionsOnMorph: aMorph fromPresentation: aPresentation
	aPresentation allActionsWithShortcuts do: [ :each |
		each installKeyCombinationForPresentation: aPresentation onMorph: aMorph ]