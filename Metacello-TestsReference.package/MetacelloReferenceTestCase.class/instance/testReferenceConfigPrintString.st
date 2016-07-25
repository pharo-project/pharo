testing
testReferenceConfigPrintString

	| version string expected x y |
	version := MetacelloReferenceConfig project version: '1.0-baseline'.
	string := self conditionResult: version spec printString.
	expected := self expectedPrintString.
	1 to: string size do: [: i |
		(i > expected size or: [ (string at: i) ~= (expected at: i) ])
			ifTrue: [ 
				x := string copyFrom: ((i- 25) max: 1) to: ((i + 25) min: string size).
				y := expected copyFrom: (((i- 25) max: 1) min: expected size) to: ((i + 25) min: expected size).
				(Array with: x with: y) "halt" ]].
	self assert: string = expected