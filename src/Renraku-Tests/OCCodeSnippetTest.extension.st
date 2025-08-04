Extension { #name : 'OCCodeSnippetTest' }

{ #category : '*Renraku-Tests' }
OCCodeSnippetTest >> testCritiques [

	| ast critiques |
	ast := snippet parse.
	critiques := ast critiques.

	snippet numberOfCritiques ifNotNil: [ :n |
		self assert: critiques size equals: n.
		^ self ].

	"Alone blocks will have ReDeadBlockRule. Currently no other critiques are fired."
	"When we get some critiques, we will add a instance vaviable in the snippet to validate them"
	ast isBlock
		ifTrue: [
			self assert: critiques size equals: 1.
			self assert: critiques anyOne rule class equals: ReDeadBlockRule ]
		ifFalse: [ self assert: critiques isEmpty ]
]
