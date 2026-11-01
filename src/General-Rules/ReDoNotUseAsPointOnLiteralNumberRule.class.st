"
I report `asPoint` sent to a literal number, and rewrite it into the point built with `@`:
`33 asPoint` becomes `33 @ 33`.

`@` states the point directly, without asking a number to convert itself.

I use the parse tree rewriter: I match `` `#literal asPoint `` and rewrite it to `` `#literal @ `#literal ``.
The pattern variable `` `#literal `` matches any literal, so `#afterCheck:mappings:` keeps only literal numbers:
`asPoint` sent to a variable, an expression, or another literal (a string, a symbol, an array) is left alone.
"
Class {
	#name : 'ReDoNotUseAsPointOnLiteralNumberRule',
	#superclass : 'ReNodeRewriteRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReDoNotUseAsPointOnLiteralNumberRule class >> groupSelector [

	^ #codingIdiomViolationGroup
]

{ #category : 'accessing' }
ReDoNotUseAsPointOnLiteralNumberRule class >> ruleName [

	^ 'Do not use #asPoint on a literal number: use @ (e.g. 33 @ 33)'
]

{ #category : 'accessing' }
ReDoNotUseAsPointOnLiteralNumberRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'DoNotUseAsPointOnLiteralNumber'
]

{ #category : 'running' }
ReDoNotUseAsPointOnLiteralNumberRule >> afterCheck: aNode mappings: mappingDict [
	"The pattern matches any literal receiver: only rewrite literal numbers."

	^ (mappingDict at: '`#literal') value isNumber
]

{ #category : 'initialization' }
ReDoNotUseAsPointOnLiteralNumberRule >> initialize [

	super initialize.
	self replace: '`#literal asPoint' with: '`#literal @ `#literal'
]
