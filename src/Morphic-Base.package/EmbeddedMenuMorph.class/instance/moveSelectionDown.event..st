keyboard control
moveSelectionDown: anInteger event: anEvent
	"Move the selection down or up (negative number) by (at least)
	the specified amount. If the item is not enabled, scan one at a time
	in that direction. If we move off the top/bottom then switch focus to any
	sibling menu and start scanning at the relevant end."

	| allEnabledSiblingItems index|
	allEnabledSiblingItems := self allEnabledSiblingItems.
	index := (allEnabledSiblingItems indexOf: selectedItem ifAbsent: [0 + (anInteger negative ifTrue: [1] ifFalse: [0])]) + anInteger.
	allEnabledSiblingItems do: "Ensure finite"
		[:unused | | m | m := allEnabledSiblingItems atWrap: index.
		((m isMenuItemMorph) and: [m isEnabled]) ifTrue:
			[m owner = self owner ifFalse: [
				anEvent hand newKeyboardFocus: m owner].
				^m owner selectItem: m event: anEvent].
		"Keep looking for an enabled item"
		index := index + anInteger sign].
	^self selectItem: nil event: anEvent