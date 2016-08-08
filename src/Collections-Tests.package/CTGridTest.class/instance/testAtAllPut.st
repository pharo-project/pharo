torevisit- tests - accessing
testAtAllPut
	| m |
	m := self gridClass new: 3.
	m do: [ :each | self assert: each equals: nil ].
	m atAllPut: 1.
	m do: [ :each | self assert: each equals: 1 ]