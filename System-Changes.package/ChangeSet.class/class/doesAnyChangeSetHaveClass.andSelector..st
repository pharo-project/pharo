services
doesAnyChangeSetHaveClass: aClass andSelector: aSelector
	"Answer whether any known change set bears a change for the given class and selector"

	^ (self countOfChangeSetsWithClass: aClass andSelector: aSelector) > 0