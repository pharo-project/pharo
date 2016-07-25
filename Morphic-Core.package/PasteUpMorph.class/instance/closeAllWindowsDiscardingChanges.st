Morphic-Base-Windows
closeAllWindowsDiscardingChanges
	World systemWindows do: [:w | [w delete] valueSupplyingAnswer: false]