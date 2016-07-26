private
constructPackagesSet
	| result |
	result := ((self environment packages collect:#name )difference: (self packagesSelection packages collect:#name)).
	^result asSet asSortedCollection: [:a :b | a<b]