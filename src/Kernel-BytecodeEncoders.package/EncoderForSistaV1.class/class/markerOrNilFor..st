compiled method support
markerOrNilFor: compiledMethod
	"What is a marker method?  It is method with body like 
		'self subclassResponsibility' or '^ self subclassResponsibility' 
	used to indicate ('mark') a special property.

	Marker methods compile to bytecode like:

		9 <70> self
		10 <D0> send: <literal 1>
		11 <87> pop
		12 <78> returnSelf

	for the first form, or 

		9 <70> self
		10 <D0> send: <literal 1>
		11 <7C> returnTop

	for the second form."

	| e |
	((e := compiledMethod endPC) = 19 or: [e = 20]) ifFalse: [^ nil].
	(compiledMethod numLiterals = 3) ifFalse:[^ nil].
	(compiledMethod at: 17) = 76 ifFalse:[^ nil].		"push self"
	(compiledMethod at: 18) = 128 ifFalse:[^ nil].		"send <literal 1>"
	"If we reach this point, we have a marker method that sends self <literal 1>"
	^ compiledMethod literalAt: 1