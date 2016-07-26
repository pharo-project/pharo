enumerating
mostRecentChangeSetWithChangeForClass: class selector: selector
	| hits |
	hits := self allChangeSets select: 
		[:cs | (cs atSelector: selector class: class) ~~ #none].
	hits isEmpty ifTrue: [^ 'not in any change set'].
	^ 'recent cs: ', hits last name