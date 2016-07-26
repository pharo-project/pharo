geometry
shiftSubmorphsOtherThan: listNotToShift by: delta
	| rejectList |
	rejectList := listNotToShift ifNil: [OrderedCollection new].
	(submorphs copyWithoutAll: rejectList) do:
		[:m | m position: (m position + delta)]