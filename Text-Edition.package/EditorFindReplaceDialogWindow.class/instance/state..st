accessing
state: aFindReplaceService
	state ifNotNil: [state removeDependent: self].
	state := aFindReplaceService.
	state ifNotNil: [state addDependent: self].
