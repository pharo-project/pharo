private
internalGetText
	| priorSource |
	priorSource := (self priorVersionOfAMethod: method) ifNotNil: [ :m | m sourceCode ] ifNil: [ '' ].
	^ Array with: priorSource with: method sourceCode