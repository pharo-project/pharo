menu builder
groupMenuBuilder

	^ PragmaMenuBuilder withAllPragmaKeywords: {self groupFixPragma. self groupPragma} model: self