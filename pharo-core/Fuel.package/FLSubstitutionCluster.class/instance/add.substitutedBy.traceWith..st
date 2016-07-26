analyzing
add: anObject substitutedBy: anotherObject traceWith: aAnalysis

	substitutions 
		at: anObject 
		ifAbsent: [ 
			substitutions at: anObject put: anotherObject.
			substitutes add: anotherObject.
			aAnalysis trace: anotherObject ].