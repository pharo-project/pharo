user interface
inputWidgetSelectorInClass: aClass 
	| selector |
	^ (aClass respondsTo: (selector := #settingInputWidgetForNode:))
		ifTrue: [selector] 