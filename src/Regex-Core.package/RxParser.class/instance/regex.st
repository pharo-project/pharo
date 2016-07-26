recursive descent
regex
	"<regex> ::= e | <branch> `|' <regex>"

	| branch regex |
	branch := self branch.
	
	(lookahead = #epsilon 
	or: [ lookahead = $) ])
		ifTrue: [ regex := nil ]
		ifFalse: [
			self match: $|.
			regex := self regex ].
		
	^RxsRegex new initializeBranch: branch regex: regex