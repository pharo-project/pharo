error correction
queryUndefined
	| varStart varName | 
	varName := parseNode key.
	varStart := self endOfLastToken + requestorOffset - varName size + 1.
	requestor selectFrom: varStart to: varStart + varName size - 1; select.
	(UndefinedVariable name: varName) ifFalse: [^ self fail]