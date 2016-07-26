user interface
inputWidgetForLabel
	| widget |
	widget := self theme newLabelIn: World for: self label: '' getEnabled: nil.
	widget getTextSelector: #preferenceValue.
	widget width: 450.
	^ widget