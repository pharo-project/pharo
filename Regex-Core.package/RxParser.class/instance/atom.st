recursive descent
atom
	"An atom is one of a lot of possibilities, see below."

	| atom |
	(lookahead = #epsilon 
	or: [ lookahead = $| 
	or: [ lookahead = $)
	or: [ lookahead = $*
	or: [ lookahead = $+ 
	or: [ lookahead = $? ]]]]])
		ifTrue: [ ^RxsEpsilon new ].
		
	lookahead = $( 
		ifTrue: [
			"<atom> ::= '(' <regex> ')' "
			self match: $(.
			atom := self regex.
			self match: $).
			^atom ].
	
	lookahead = $[
		ifTrue: [
			"<atom> ::= '[' <characterSet> ']' "
			self match: $[.
			atom := self characterSet.
			self match: $].
			^atom ].
	
	lookahead = $: 
		ifTrue: [
			"<atom> ::= ':' <messagePredicate> ':' "
			self match: $:.
			atom := self messagePredicate.
			self match: $:.
			^atom ].
	
	lookahead = $. 
		ifTrue: [
			"any non-whitespace character"
			self next.
			^RxsContextCondition new beAny].
	
	lookahead = $^ 
		ifTrue: [
			"beginning of line condition"
			self next.
			^RxsContextCondition new beBeginningOfLine].
	
	lookahead = $$ 
		ifTrue: [
			"end of line condition"
			self next.
			^RxsContextCondition new beEndOfLine].
		
	lookahead = $\ 
		ifTrue: [
			"<atom> ::= '\' <character>"
			self next.
			lookahead = #epsilon 
				ifTrue: [ self signalParseError: 'bad quotation' ].
			(BackslashConstants includesKey: lookahead)
				ifTrue: [
					atom := RxsCharacter with: (BackslashConstants at: lookahead).
					self next.
					^atom].
			self ifSpecial: lookahead
				then: [:node | self next. ^node]].
		
	"If passed through the above, the following is a regular character."
	atom := RxsCharacter with: lookahead.
	self next.
	^atom