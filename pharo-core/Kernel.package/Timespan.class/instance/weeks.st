enumerating
weeks


	| weeks |
	weeks := OrderedCollection new.
	self weeksDo: [ :m | weeks add: m ].
	^ weeks asArray.