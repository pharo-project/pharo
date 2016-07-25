instance-creation
newButtonsMorph
	"Answer a new buttons morph."

	^(self newRow: {
			self newConflictsButton.
			self newToolSpacer hResizing: #spaceFill.
			self newMergeButton.
			self newCancelButton})
		removeProperty: #fillStyle;
		listCentering: #bottomRight;
		layoutInset: 4