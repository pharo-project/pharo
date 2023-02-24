Extension { #name : #RBCodeSnippetTest }

{ #category : #'*Renraku-Tests' }
RBCodeSnippetTest >> testCritiques [

	| ast critiques |
	ast := snippet parse.
	critiques := ast critiques.

	"Alone blocks will have ReDeadBlockRule. Currently no other critiques are fired."
	"When we get some critiques, we will add a instance vaviable in the snipet to validate them"
	ast isBlock
		ifTrue: [
			self assert: critiques size equals: 1.
			self assert: critiques anyOne rule class equals: ReDeadBlockRule ]
		ifFalse: [ self assert: critiques isEmpty ]
]
