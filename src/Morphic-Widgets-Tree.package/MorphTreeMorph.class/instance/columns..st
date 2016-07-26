column handling
columns: aListOfTreeColumn
	"set the columns -  as a consequence, the topHeader is update (if present) and column resizers are added"

	self removeColumnResizers.
	topHeader ifNotNil: [ self removeMorph: topHeader ].
	columns := aListOfTreeColumn asOrderedCollection.
	aListOfTreeColumn do: [:col | col container: self].
	self buildTopHeader .
	self addColumnResizers.
