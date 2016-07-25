private-actions
tabSelected: aTab

	self isMultiSelection
		ifTrue: [ self tabResetSelection: aTab ]
		ifFalse: [ 
			self selectedTab ifNotNil: [ self selectedTab selected: false ].
			self addInHistory: aTab.	
			self adjustLayout.
			self triggerEvent: #tabSelected with: aTab ]