private
wrap: item at: index
	| checkBox |

	associationsIndexToWrapper 
		at: index
		ifPresent: [:cb | ^ cb ].

	checkBox := self instantiate: CheckBoxModel.
	blockToPerformOnWrappers value value: checkBox.
	checkBox label: (wrapHolder value cull: item cull: self).
	
	checkBox whenActivatedDo: [ 
		self addPicked: item.
		listModel setSelectedItem: checkBox.
		listModel takeKeyboardFocus ].
	
	checkBox whenDeactivatedDo: [ 
		self removePicked: item.
		listModel setSelectedItem: checkBox.
		listModel takeKeyboardFocus ].
	
	checkBox 
		state: self defaultValue;
		labelClickable: self labelClickable.
		
	associationsWrapperToItem at: checkBox put: item.
	associationsItemToWrapper at: item put: checkBox.
	associationsIndexToWrapper at: index put: checkBox.
	^ checkBox