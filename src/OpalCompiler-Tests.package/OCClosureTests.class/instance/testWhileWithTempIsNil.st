testing-while
testWhileWithTempIsNil
	"self debug: #testWhileWithTempIsNil"
	| index |
	index := 0.
	[ index < 5 ] whileTrue: [
		| temp |
		collection add: temp.
		temp := index := index + 1. 
		collection add: temp].
	self assertValues: #(nil 1 nil 2 nil 3 nil 4 nil 5)