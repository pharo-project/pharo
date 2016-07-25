private
resolve
	example := (self owner finder copy
		subjects: nil; " do not inherit subjects from the owner -> #initializeFromFinder:withPragma: "
		yourself) proxy: self.
	example ifNotNil: [ example parents add: self ]