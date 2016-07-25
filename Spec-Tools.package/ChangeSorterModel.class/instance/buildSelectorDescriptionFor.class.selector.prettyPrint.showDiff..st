text
buildSelectorDescriptionFor: changeSet class: class selector: selector prettyPrint: prettyPrint showDiff: showDiff

	| changeType code |
	changeType := changeSet atSelector: selector class: class.
	
	changeType == #remove
		ifTrue: [^ 'Method has been removed (see versions)'].
	changeType == #addedThenRemoved
		ifTrue: [^ 'Added then removed (see versions)']. 
	class
		ifNil: [^ 'Method was added, but cannot be found!'].
	(class includesSelector: selector)
		ifFalse: [^ 'Method was added, but cannot be found!'].
	code := class sourceCodeAt: selector.
	prettyPrint
		ifTrue: [ code := class compiler format: code].						
	showDiff
		ifTrue: [ code := self diffFromPriorSourceFor: code ].
		
	^ code asText