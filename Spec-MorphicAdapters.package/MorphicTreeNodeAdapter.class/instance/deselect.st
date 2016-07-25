private
deselect

	self widgetDo: [ :w |
		w changed: #deselect.
		w model selectionChanged.
		w dependents do: [ :e | e changed ] ]