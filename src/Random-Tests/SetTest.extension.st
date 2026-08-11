Extension { #name : 'SetTest' }

{ #category : '*Random-Tests' }
SetTest >> testAtRandom [
	| rand |
	rand := Random new.
	self full add: 3.
	self full add: 2.
	self full add: 4.
	self full add: 1.
	self assert: (self full includes: (self full atRandom: rand))
]
