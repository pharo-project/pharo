tests
testEnumerating
	| weeks |
	weeks := OrderedCollection new.
	month weeksDo: [ :w | weeks add: w start ].
	0 to: 4 do: [ :i | weeks remove: (Week starting:  ('29 June 1998' asDate addDays: i * 7)) start ].
	self assert: weeks isEmpty