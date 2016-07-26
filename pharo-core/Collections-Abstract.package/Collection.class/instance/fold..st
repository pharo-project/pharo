enumerating
fold: binaryBlock
	"Evaluate the block with the first two elements of the receiver,
	then with the result of the first evaluation and the next element,
	and so on.  Answer the result of the final evaluation. If the receiver
	is empty, raise an error. If the receiver has a single element, answer
	that element."
	"#('if' 'it' 'is' 'to' 'be' 'it' 'is' 'up' 'to' 'me') fold: [:a :b | a, ' ', b]"

	^self reduce: binaryBlock