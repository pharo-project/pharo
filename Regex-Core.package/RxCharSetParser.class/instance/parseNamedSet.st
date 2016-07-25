parsing
parseNamedSet

	| name |
	self match: $[; match: $:.
	name := (String with: lookahead), (source upTo: $:).
	lookahead := source next.
	self match: $].
	elements add: (RxsPredicate forNamedClass: name)