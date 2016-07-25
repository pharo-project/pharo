user interface
inputWidgetForString
	| widget |
	widget := EditableDropListMorph
		on: self
		list: #list
		selected: #index
		changeSelected: #index:
		useIndex: true
		addToList: #addToList:
		class: (Smalltalk globals at: self type)
		getEnabled: #enabled
		default: self defaultValue.
	widget ghostText: self ghostHelp.
	widget contentMorph wantsFrameAdornments: true.
	widget hResizing: #rigid.
	widget width: 450.
	^ widget