public
slowSize
	"Careful! Answer the maximum amount
	of elements in the receiver, not the
	exact amount"

	| count |
	count := 0.
	1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object == flag ifFalse: [
				count := count + 1 ] ] ].
	^count