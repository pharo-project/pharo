testing
isOverridden
	| selector| 
	selector := self inlineSelector.
	self inlineClass allSubclassesDo: [:each | 
		(each directlyDefinesMethod: selector)
			ifTrue: [ ^ true ]].
	^ false