tests
testEnumerating

	| days |
	days := OrderedCollection new.
	0 to: 6 do: [ :i | days add: ('28 June 1998' asDate addDays: i) ].

	week datesDo: [ :d | days remove: d ].
	
	self assert: days isEmpty.
