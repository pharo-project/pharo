private
wrap: item at: index
	| checkBox |

	associationsIndexToWrapper 
		at: index
		ifPresent: [:cb | ^ cb ].

	checkBox := self instantiate: CheckBoxPresenter.
	blockToPerformOnWrappers value value: checkBox.
	checkBox label: (wrapHolder value cull: item cull: self).
	
	checkBox whenActivatedDo: [ 
		self addPicked: item.
		listPresenter setSelectedItem: checkBox.
		listPresenter takeKeyboardFocus ].
	
	checkBox whenDeactivatedDo: [ 
		self removePicked: item.
		listPresenter setSelectedItem: checkBox.
		listPresenter takeKeyboardFocus ].
	
	checkBox 
		state: self defaultValue;
		labelClickable: self labelClickable.
		
	associationsWrapperToItem at: checkBox put: item.
	associationsItemToWrapper at: item put: checkBox.
	associationsIndexToWrapper at: index put: checkBox.
	^ checkBox