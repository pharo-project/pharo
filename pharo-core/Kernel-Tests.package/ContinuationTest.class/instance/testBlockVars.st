tests
testBlockVars
	| continuation |
	tmp := 0.
	tmp := (self callcc: [ :cc | continuation := cc. 0 ]) + tmp.
	tmp2
		ifNotNil: [ tmp2 value ]
		ifNil: [
			#(1 2 3) do: [ :i |
				self callcc: [ :cc | tmp2 := cc. continuation value: i ] ] ].
	self assert: tmp = 6