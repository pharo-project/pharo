building widgets
dynamicLayoutForActions: aCollection

	^ SpecLayout composed
		newRow: [ :r |
			aCollection do: [ :action | 
				r add: action specId ] ]