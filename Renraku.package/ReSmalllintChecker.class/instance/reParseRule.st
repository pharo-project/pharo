as yet unclassified
reParseRule

	methodRules  := Set new.
	nodeRules    := Set new.
	classRules   := Set new.
	packageRules := Set new.

	rule leaves do: [ :r |
		r class checksMethod  ifTrue: [ methodRules  add: r ].
		r class checksNode    ifTrue: [ nodeRules    add: r ].
		r class checksClass   ifTrue: [ classRules   add: r ].
		r class checksPackage ifTrue: [ packageRules add: r ] ]