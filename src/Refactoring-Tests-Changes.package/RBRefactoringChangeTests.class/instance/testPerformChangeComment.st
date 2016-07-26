tests-perform
testPerformChangeComment
	| change comment |
	change := changes comment: 'Some Comment' in: self changeMock.
	comment := change changeClass organization classComment.
	self perform: change do: [ self assert: (change changeClass comment = 'Some Comment') ].
	self assert: change changeClass organization classComment = comment