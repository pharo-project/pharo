recursive descent
lookAround
	"Parse a lookaround expression after: (?<lookround>) 
	<lookround> ::= !<regex> | =<regex>"
	| lookaround |
	(lookahead = $!
	or: [ lookahead = $=])
		ifFalse: [ ^ self signalParseError: 'Invalid lookaround expression ?', lookahead asString ].
	self next.
	lookaround := RxsLookaround with: self regex.
	lookahead = $!
		ifTrue: [ lookaround beNegative ].
	^ lookaround
	