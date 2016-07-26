menu builder
methodMenuBuilder

	^ PragmaMenuBuilder withAllPragmaKeywords: {self commonPragma. self methodPragma} model: self