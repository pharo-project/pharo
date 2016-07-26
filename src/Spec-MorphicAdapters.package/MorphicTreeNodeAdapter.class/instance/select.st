private
select

	self widgetDo: [ :w |
		w changed: #select.
		w model selectionChanged.
		w dependents do: [ :e | e changed ] ]