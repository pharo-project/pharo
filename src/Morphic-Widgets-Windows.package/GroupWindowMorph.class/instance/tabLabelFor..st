windows
tabLabelFor: aSystemWindow
	"Answer the tab label to use for the given page."

	^self newRow: {
		 (self newButtonLabel: (aSystemWindow labelString truncateWithElipsisTo: 40))
			setBalloonText: aSystemWindow labelString.
		 self
			newCloseControlFor: nil
			action: [self removeWindow: aSystemWindow]
			help: 'Close this tab and free the window'}