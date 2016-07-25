enumerating
allChangeSetsWithClass: class selector: selector
	class ifNil: [^ #()].
	^ self allChangeSets select: 
		[:cs | (cs atSelector: selector class: class) ~~ #none]