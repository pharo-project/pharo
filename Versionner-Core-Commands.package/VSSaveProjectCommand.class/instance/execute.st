execute
execute
	| configClassName |
	target
		ifNil: [ self inform: 'Select the version to commit.'.
			^ self ].
	configClassName := self configurationClass name asString.
	self class validateBeforeSaving
		ifTrue: [ (MetacelloToolBox checkForCriticalValidationIssues: configClassName)
				ifEmpty: [ self commit]
				ifNotEmpty: [ :errors | self alertValidationErrors: errors for: configClassName ] ]
		ifFalse: [ self commit ]