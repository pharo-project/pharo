menu builder
categoryMenuBuilder

	^ PragmaMenuBuilder withAllPragmaKeywords: {self commonPragma. self categoryPragma} model: self