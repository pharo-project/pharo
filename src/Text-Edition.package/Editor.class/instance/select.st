current selection
select
	"If the text selection is visible on the screen, reverse its highlight."
	^'Not doing anything'
	"screw this logic
	selectionShowing ifFalse: [self reverseSelection]"