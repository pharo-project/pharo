accessing
namesAt: aName ifAbsent: aBlock
	"To avoid creating the names map if empty"

	^ NamesMap ifNil: aBlock ifNotNil: [ NamesMap at: aName ifAbsent: aBlock ]