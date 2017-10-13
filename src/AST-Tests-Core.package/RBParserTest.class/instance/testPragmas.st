tests parsing
testPragmas
	| tree node |
	#(('foo <foo>' 				#foo ()) 
	('foo <foo: 1>' 				#foo: (1)) 
	('foo <foo: 1.2>' 			#foo: (1.2)) 
	('foo <foo: -3>' 			#foo: (-3)) 
	('foo <foo: #a>' 			#foo: (a)) 
	('foo <foo: #< >' 			#foo: (<)) 
	('foo <foo: #> >' 			#foo: (>)) 
	('foo <foo: $a>' 			#foo: ($a)) 
	('foo <foo: ''bar''>' 		#foo: ('bar')) 
	('foo <foo: true>' 			#foo: (true)) 
	('foo <foo: false>' 			#foo: (false)) 
	('foo <foo: nil>' 			#foo: (nil)) 
	('foo <foo: #(1 2)>' 		#foo: ((1 2)))
	('foo <foo: 1 bar: 2>' 		#foo:bar: (1 2))
	('foo |t| <foo: 1.2>' 		#foo: (1.2))
	('foo |t| <foo: 1 bar: 2>' #foo:bar: (1 2))
	) do: [ :each |
		tree := self parserClass parseMethod: each first.
		self assert: (tree pragmas size = 1).
		node := tree pragmas first.
		self assert: node selector = each second.
		1 to: node arguments size do: [ :i |
			self assert: (node arguments at: i) value = (each last at: i) ].
		self assert: (each first at: node start) equals: $<.
		self assert: (each first at: node stop) equals: $> ].
	
	self assert: (self parserClass parseMethod: 'selector <a>') isMethod.
	self assert: (self parserClass parseMethod: '
	selector 
		"comment1" 
		|temp| 
		"comment2" 
		<a> 
		#<.
		^1') isMethod.
	self should: [self parserClass parseMethod: '
	selector 
		"comment1" 
		|temp| 
		"comment2" 
		<a> 
		<.
		^1'] raise: SyntaxErrorNotification.