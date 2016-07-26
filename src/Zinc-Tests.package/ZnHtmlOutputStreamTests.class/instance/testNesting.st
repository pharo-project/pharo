tests
testNesting
	| result |
	result := ZnHtmlOutputStream streamContents: [ :html |
		html tag: #ol do: [ 
			#(one two three) do: [ :each | html tag: #li with: each ] ] ].
	self assert: result equals: '<ol><li>one</li><li>two</li><li>three</li></ol>'