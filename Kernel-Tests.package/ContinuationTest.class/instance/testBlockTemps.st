tests
testBlockTemps
	| y |
	#(1 2 3) do: [ :i |
		| x |
		x := i.
		tmp ifNil: [ tmp2 := (self callcc: [ :cc | tmp := cc. [ :q | ] ]) ].
		tmp2 value: x.
		x := 17 ].
	y := (self callcc: [ :cc | tmp value: cc. 42 ]).
	self assert: y = 1