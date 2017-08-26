tests comparing
testEquivalentExceptRenaming
	#(('a 3-4' 'a 4-3' false ) ('a #[3 4]' 'a #(3 4)' false ) ('a variable1 ~~ "comment" variable2' 'a variable1 ~~ variable2' true ) ('a variable1' 'a variable2' false ) ('a [:a :b | a + b]' 'a [:b :a | a + b]' false ) ('a | a b | a + b' 'a | b a | a + b' true ) ('a | a | a msg1; msg2' 'a | b | b msg2; msg2' false ) ('a c' 'a d' true ) ('a | a b | a := b. ^b msg1' 'a | a b | b := a. ^a msg1' true ) ('a | a b | a := b. ^b msg1: a' 'a | a b | b := a. ^b msg1: a' false ) ('a: b b + 4' 'a: e e + 4' true ) ('a: b b + 4' 'b: b b + 4' false ) ('a: b b: c b + c' 'a: c b: b c + b' true ) ('a: a b: b a + b' 'a: b b: a a + b' false ) ) do: [:each | self assert: ((self parserClass parseMethod: each first)
				equalTo: (self parserClass parseMethod: (each at: 2))
				exceptForVariables: #('c' ))
				== each last ]