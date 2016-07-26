accessing
enabledFilters
	| enabledFilters |
	
	enabledFilters := OrderedCollection new.
	self filterDoItSelectors ifTrue: [ 
		enabledFilters addLast: self doItFilter ].
	self filterKernelClasses ifTrue: [ 
		enabledFilters addLast: self kernelClassesFilter ].
	self filterNilSelectors ifTrue: [ 
		enabledFilters addLast: self nilMsgsFilter ].
	self filterLinkSelectors ifTrue: [ 
	enabledFilters addLast: self linkMessagesFilter].	
	^ enabledFilters
 