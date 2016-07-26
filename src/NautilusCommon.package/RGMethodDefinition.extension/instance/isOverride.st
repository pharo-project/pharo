*NautilusCommon
isOverride
	| selector |
	selector := self selector.
	self methodClass allSuperclassesDo: [:each | 
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false