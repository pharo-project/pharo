other
replacedBy: aForm do: aBlock
	"Permits normal display to draw on aForm instead of the display."

	ScreenSave := self.
	Display := aForm.
	aBlock value.
	Display := self.
	ScreenSave := nil.