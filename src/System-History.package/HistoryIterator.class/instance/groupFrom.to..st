grouping
groupFrom: firstIdx to: secondIdx
	| group |
	group := UndoRedoGroup new.
	firstIdx to: (secondIdx min: index) do: [:i | group addItem: (self at:i)].
	group close.
	group 
		isEmpty ifTrue: [^ self].
	firstIdx + 1 to: (secondIdx min: index) do: [:i | self removeAt: i].
	self at: firstIdx put: group.
	index := index - group size + 1.