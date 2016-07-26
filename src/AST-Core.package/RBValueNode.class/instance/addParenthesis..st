accessing
addParenthesis: anInterval 
	parentheses ifNil: [parentheses := OrderedCollection new: 1].
	parentheses add: anInterval