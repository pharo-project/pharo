critic creation
critic: aCriticClass for: ast at: node regarding: sequence

	^ aCriticClass 
		withAnchor: (
			ReIntervalSourceAnchor
				entity: ast
				interval: node sourceInterval)
		by: self
		method: node selector
		condition: sequence
		