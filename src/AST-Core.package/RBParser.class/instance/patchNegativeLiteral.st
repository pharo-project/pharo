private
patchNegativeLiteral
	"Handle the special negative number case for binary message sends."

	currentToken value isNumber 
		ifFalse: [^self].
	currentToken value <= 0 ifFalse: [^self].
	currentToken value = 0 
		ifTrue: 
			[(source notNil and: 
					[source notEmpty 
						and: [(source at: (currentToken start min: source size)) = $-]]) 
				ifFalse: [^self]].
	nextToken := currentToken.
	currentToken := RBBinarySelectorToken value: #- start: nextToken start.
	nextToken value: nextToken value negated.
	(nextToken isKindOf: RBNumberLiteralToken) 
		ifTrue: 
			[nextToken source: (nextToken source copyFrom: 2 to: nextToken source size)].
	nextToken start: nextToken start + 1