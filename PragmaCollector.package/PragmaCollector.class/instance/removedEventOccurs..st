system changes
removedEventOccurs: anEvent
	"a method has been removed, remove any corresponding handler if found"

	(self
		detect: [ :prag | 
			prag methodSelector = anEvent selector
				and: [ prag methodClass = anEvent methodClass ] ])
		ifNotNil: [ :found | self removePragma: found ]