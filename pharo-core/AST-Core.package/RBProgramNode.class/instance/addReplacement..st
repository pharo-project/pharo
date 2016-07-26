replacing
addReplacement: aStringReplacement
	parent ifNil: [^self].
	parent addReplacement: aStringReplacement