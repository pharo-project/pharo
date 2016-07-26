initialization
withDropListButton
	| downArrow downArrowBtn |
	endRow := AlignmentMorph newRow.
	downArrow := (self theme 
		basicCreateArrowOfDirection: #bottom 
		size: (textMorph textStyle defaultFont height)
		color: Color veryLightGray).
	downArrowBtn := IconicButton new labelGraphic: downArrow.
	downArrowBtn height: self innerBounds height.
	downArrowBtn borderWidth: 0.
	downArrowBtn color: Color transparent.
	downArrowBtn actionSelector:  #openChooserWithAllOrCloseFrom:.
	downArrowBtn target: entryCompletion.
	downArrowBtn arguments: (Array with: self).
	endRow height: downArrowBtn height.
	endRow width: downArrowBtn width.
	endRow color: Color transparent.
	endRow addMorph: downArrowBtn.
	self addMorph: endRow.