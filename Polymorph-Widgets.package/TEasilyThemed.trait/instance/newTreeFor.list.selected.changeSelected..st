controls
newTreeFor: aModel list: listSelector selected: getSelector changeSelected: setSelector
	"Answer a new tree morph."
	
	^self theme
		newTreeIn: self
		for: aModel
		list: listSelector
		selected: getSelector
		changeSelected: setSelector