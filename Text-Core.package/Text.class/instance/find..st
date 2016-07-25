emphasis
find: attribute
	"Return the first interval over which this attribute applies"
	| begin end |
	begin := 0.
	runs withStartStopAndValueDo:
		[:start :stop :attributes |
		(attributes includes: attribute)
			ifTrue: [begin = 0 ifTrue: [begin := start].
					end := stop]
			ifFalse: [begin > 0 ifTrue: [^ begin to: end]]].
	begin > 0 ifTrue: [^ begin to: end].
	^ nil