stepping and presenter
step
	"Reset mouse focus to the list if it is showing."

	self listVisible ifTrue: [
		self activeHand mouseFocus ifNil: [
			 self listMorph wantsKeyboardFocus ifTrue: [
				self listMorph takeKeyboardFocus].
			self activeHand newMouseFocus: self listMorph]]