Extension { #name : #SetTest }

{ #category : #'*Random-Tests' }
SetTest >> testAtRandom [
	| rand |
	rand := Random new.
	full add: 3.
	full add: 2.
	full add: 4.
	full add: 1.
	self assert: (full includes: (full atRandom: rand)).
	
]
