instance creation
class: aClass temporaries: aCollection additionals: additionalCollection variables: variablesBoolean selectors: selectorsBoolean 
	| newInstance |
	newInstance := self basicNew initialize.
	newInstance 
		setClass: aClass
		temporaries: aCollection
		additionals: additionalCollection
		variables: variablesBoolean
		selectors: selectorsBoolean.
	^ newInstance