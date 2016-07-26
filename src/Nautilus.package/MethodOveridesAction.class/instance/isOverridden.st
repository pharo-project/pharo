order
isOverridden
	| selector| 
	selector := method selector.
	method methodClass allSubclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false
	