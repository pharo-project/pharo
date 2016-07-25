keyboard control
moveSelectionDown: direction event: evt
	"Move the current selection up or down by one, presumably under keyboard control.
	direction = +/-1"

	| index |
	index := (submorphs indexOf: selectedItem ifAbsent: [1-direction]) + direction.
	submorphs do: "Ensure finite"
		[:unused | | m | m := submorphs atWrap: index.
		((m isMenuItemMorph) and: [m isEnabled]) ifTrue:
			[^ self selectItem: m event: evt].
		"Keep looking for an enabled item"
		index := index + direction sign].
	^ self selectItem: nil event: evt