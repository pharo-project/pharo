private
match: aPattern for: aCollection

	| matcher matches |
	
	aPattern isEmpty ifTrue: [ ^ aCollection ].
	
	matcher := FuzzyMatcher pattern: aPattern.

	matches := OrderedCollection new: aCollection size.
	
	aCollection do: [ :each | 
		matcher 
			match: each key
			ifScored: [ :score | matches add: each key -> score ] 
	].
	
	matches sort: [ :a :b | a value >= b value ].
	
	^ matches