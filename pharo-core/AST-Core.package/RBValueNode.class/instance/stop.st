accessing
stop
	^ parentheses ifNil: [ self stopWithoutParentheses ] ifNotNil: [ parentheses last last ]