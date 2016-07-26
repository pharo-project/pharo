tests
testReferenceSharingIgnore
	| serializer array one |
	serializer := [ :object | 
		String streamContents: [ :stream |
			STON writer 
				on: stream;
				referencePolicy: #ignore; 
				nextPut: object ] ].
	one := { #one }.
	array := STON listClass with: one with: one with: one.
	self assert: (serializer value: array) = '[[#one],[#one],[#one]]'.