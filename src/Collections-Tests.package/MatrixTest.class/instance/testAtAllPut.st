tests - accessing
testAtAllPut

	| m | 
	m := Matrix new: 3. 
	m do: [ :each | self assert: each equals: nil ].
	m atAllPut: 1.
	m do: [ :each | self assert: each equals: 1 ].