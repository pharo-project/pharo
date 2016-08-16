accessing
pragmaMapping
	"use a class inst var so subclasses can define custom mappings"
	^ pragmaMapping ifNil: [
		self buildPragmaMapping.
		pragmaMapping]