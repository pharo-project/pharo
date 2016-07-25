initialize-release
setClass: aClass temporaries: aCollection additionals: additionalCollection variables: variablesBoolean selectors: selectorsBoolean 
	includeVariables := variablesBoolean.
	includeSelectors := selectorsBoolean.
	clazz := aClass.
	self initializeInstVars.
	self initializeClassVars.
	includeVariables 
		ifTrue: [ 
			localVars := aCollection.
			self addAdditionals: additionalCollection ]
		ifFalse: [ localVars := OrderedCollection new ].
	self resetEntries.