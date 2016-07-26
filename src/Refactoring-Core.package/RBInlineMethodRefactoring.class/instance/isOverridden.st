testing
isOverridden
	| selector| 
	selector := self inlineSelector.
	class allSubclassesDo: [:each | 
		(each directlyDefinesMethod: selector)
			ifTrue: [ ^ true ]].
	^ false
	
		