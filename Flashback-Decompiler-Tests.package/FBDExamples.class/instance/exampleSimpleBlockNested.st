examples-blocks
exampleSimpleBlockNested
	| a match dict | 

	a  := #(a b c d).
	dict := Dictionary new.


	a doWithIndex: [:each :index |
		(match := a indexOf: each) > 0 ifTrue:
			[dict at: index put: (a at: match)]].

	^ dict.