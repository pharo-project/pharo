enumerating
combinations
	"Return all the combinations of elements of the receiver. Note that comabinations does not include the empty element contrary to the mathematical definition of combinations."

	"#(1 2 3 4) combinations 
	returns #(#(1) #(2) #(3) #(4) #(1 2) #(1 3) #(1 4) #(2 3) #(2 4) #(3 4) #(1 2 3) #(1 2 4) #(1 3 4) #(2 3 4) #(1 2 3 4))"

	^ Array
		streamContents: [ :stream | 
			1 to: self size do: [ :take | 
					self combinations: take atATimeDo: [ :combination | stream nextPut: combination copy ] ] ]