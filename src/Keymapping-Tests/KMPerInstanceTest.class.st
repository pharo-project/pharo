"
SUnit tests for key mapping per instance
"
Class {
	#name : 'KMPerInstanceTest',
	#superclass : 'AbstractKeymappingTest',
	#category : 'Keymapping-Tests',
	#package : 'Keymapping-Tests'
}

{ #category : 'tests' }
KMPerInstanceTest >> testAddMoreThanOneHandler [

	| flag morph |
	flag := 0.
	morph := Morph new.
	morph bindKeyCombination: $i toAction: [ flag := flag + 1 ].
	morph
		on: $j
		do: [ flag := flag + 6 ].
	{
		(self eventKey: $i).
		(self eventKey: $j) } do: [ :e | morph dispatchKeystrokeForEvent: e ].
	self assert: flag equals: 7
]
