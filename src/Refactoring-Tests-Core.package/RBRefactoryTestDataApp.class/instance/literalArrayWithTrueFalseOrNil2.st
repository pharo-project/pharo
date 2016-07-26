lint
literalArrayWithTrueFalseOrNil2
	| b c |
	b := #(#true #false #nil).
	c := #(#true (#true #false #nil) #false #nil).
^b