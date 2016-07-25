accessing
stashDirectory: aStringOrFileReference

	stashDirectory := self 
							ensureDirectory: aStringOrFileReference 
							distinctFrom: self defaultStashDirectory