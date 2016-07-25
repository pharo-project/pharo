menu builder
classMenuBuilder

	^ PragmaMenuBuilder withAllPragmaKeywords: {self classFixPragma. self classPragma} model: self