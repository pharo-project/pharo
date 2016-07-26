private-extensions-flsubstitutioncluster
add: anObject substitutedBy: anotherObject traceWith: aAnalysis
	<flExtensionOf: #FLSubstitutionCluster isSource: true>
	^ 'add: anObject substitutedBy: anotherObject traceWith: aAnalysis

		substitutions 
			at: anObject 
			ifAbsent: [ 
				substitutions at: anObject put: anotherObject.
				(substitutes identityIncludes: anObject) ifFalse:  [substitutes add: anotherObject].
				aAnalysis trace: anotherObject ]'