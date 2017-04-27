testing
shouldShowActionbar
	"Return true if actionbar should be shown, false otherwise"
	<return: #Boolean>

	^ self isHovered or: [
		self isChecked or: [
			self stepModel isSelected: self candidateLink ] ]