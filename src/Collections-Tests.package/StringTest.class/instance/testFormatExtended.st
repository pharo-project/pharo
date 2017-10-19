tests - formatting
testFormatExtended

	self
		assert: ('This is {foo} !' format: (Dictionary with: #foo->'a test'))
		equals: 'This is a test !'.

 	{ 
 		[ '\{ \} \\ foo {foo} bar {bar}' format: { #foo->12. #bar->'string' } asDictionary ] -> '{ } \ foo 12 bar string'. 
 		[ '\{ \} \\ foo {bar} bar {foo}' format: { 'bar'->12. 'foo'->'string' } asDictionary ] -> '{ } \ foo 12 bar string'. 
 		[ '\{x}' format: {} ] -> '{x}'. 
 		[ '\{x}{x}' format: { #x->$a } asDictionary ] -> '{x}a'. 
 	} do: [ :each | 
		self assert: each key value equals: each value ]