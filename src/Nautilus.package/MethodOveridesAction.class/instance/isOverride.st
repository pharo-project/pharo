order
isOverride
	| selector |
	selector := method selector.
	method methodClass allSuperclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false