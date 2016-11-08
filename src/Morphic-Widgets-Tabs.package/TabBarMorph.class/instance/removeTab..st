private-actions
removeTab: aTab
	| nextSelection |

	(aTab selected and: [selectionHistory size > 1])
		ifTrue: [ 
			nextSelection := selectionHistory at: selectionHistory size - 1.
			nextSelection selected: true ].
		
	self removeFromHistory: aTab.
	tabs remove: aTab.

	self adjustLayout