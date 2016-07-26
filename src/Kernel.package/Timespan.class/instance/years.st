enumerating
years


	| years |
	years := OrderedCollection new.
	self yearsDo: [ :m | years add: m ].
	^ years asArray.