accessing
start
	^ parentheses ifNil: [ self startWithoutParentheses ] ifNotNil: [ parentheses last first ]