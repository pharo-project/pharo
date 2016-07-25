testing
isOverridden
	| selector| 
	selector := self selector.
	self methodClass allSubclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false
	