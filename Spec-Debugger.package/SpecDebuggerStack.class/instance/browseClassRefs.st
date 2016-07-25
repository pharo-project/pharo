actions browsing
browseClassRefs

	self currentContext ifNotNil: [ :aContext || class|
		class := aContext methodClass theNonMetaClass.
		class isTrait 
			ifTrue: [ self systemNavigation browseAllUsersOfTrait: class ]
			ifFalse: [ self systemNavigation browseAllCallsOnClass: class ] ]
