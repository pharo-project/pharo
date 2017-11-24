Extension { #name : #SetTest }

{ #category : #'*Random-Tests' }
SetTest >> testAtRandom [
	| rand |
	rand := Random new.
	self assert: (full includes: (full atRandom: rand)).
	
]
