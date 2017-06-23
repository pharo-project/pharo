querying
upstream
	self withRepoDo: [ :repo | ^ repo head upstream ifNotNil: #name ]