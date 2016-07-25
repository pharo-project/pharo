lint
literalArrayWithTrueFalseOrNil
	| a b c |
	a := #(true false nil).
	b := #(#true #false #nil).
	c := {true. false. nil}.
	^{a. b. c}