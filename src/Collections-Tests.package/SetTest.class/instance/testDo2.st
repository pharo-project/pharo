tests - some
testDo2
	| newFull result |
	newFull := Set withAll: (1 to: 5).
	result := 0.
	newFull do: [:each | result := (result + each)].
	self assert: (result = 15).