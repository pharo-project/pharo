Extension { #name : 'SequenceableCollection' }

{ #category : '*Random-Core' }
SequenceableCollection >> atRandom: aGenerator [
	"Answer a random element of the receiver.  Uses aGenerator which
	should be kept by the user in a variable and used every time. Use
	this instead of #atRandom for better uniformity of random numbers
	because only you use the generator.  Causes an error if self has no
	elements."

	^ self at: (aGenerator nextInteger: self size)
]

{ #category : '*Random-Core' }
SequenceableCollection >> shuffle [
	"Modify the receiver but with its elements in random positions.
	This method use Random class as random generator"

	^ self shuffleBy: Random new
]

{ #category : '*Random-Core' }
SequenceableCollection >> shuffleBy: aRandom [
	"Durstenfeld's version of the Fisher-Yates shuffle"
	"({1. 2. 3. 4. 5} shuffleBy: (Random seed: 42)) >>> #(1 2 5 4 3)"

	self size to: 2 by: -1 do: [ :i |
		self swap: i with: (aRandom nextInteger: i) ]
]

{ #category : '*Random-Core' }
SequenceableCollection >> shuffled [
	^ self copy shuffle
]

{ #category : '*Random-Core' }
SequenceableCollection >> shuffledBy: aRandom [
	"Durstenfeld's version of the Fisher-Yates shuffle"
	"({1. 2. 3. 4. 5} shuffleBy: (Random seed: 42)) >>> #(1 2 5 4 3)"

	^ self copy shuffleBy: aRandom
]
