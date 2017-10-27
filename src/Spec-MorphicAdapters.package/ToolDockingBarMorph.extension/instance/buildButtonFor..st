*Spec-MorphicAdapters
buildButtonFor: item

	^ Smalltalk ui theme
		newButtonIn: self for: item
		getState: nil
		action: #performMenuActionWith:
		arguments: { self }
		getEnabled: #enabled 
		label: (IconicListItem text: item name icon: item icon)
		help: item description 