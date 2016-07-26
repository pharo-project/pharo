accessing
allResolved
	"Answer all sorted references within the configured repositories."
	
	| resolved |
	resolved := OrderedCollection new.
	self repositories
		do: [ :repository | resolved addAll: (self allResolvedIn: repository) ].
	^ resolved asSortedCollection asArray