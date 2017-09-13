running
check: aMethod forCritiquesDo: aCriticBlock
	| returnsBool returnsNonBool |
	returnsBool := false.
	"no return in the end? -> returns self"
	returnsNonBool := aMethod ast lastIsReturn not.
	
	aMethod ast nodesDo: [ :node | 
		node isReturn ifTrue: [
			| returnedNode |
			returnedNode := node value.
			
			returnsBool :=
				returnsBool or:
				[ self checkIfNodeIsBool: returnedNode ].
			returnsNonBool :=
				returnsNonBool or:
				[ self checkIfNodeIsNotBool: returnedNode ].

			returnsBool & returnsNonBool ifTrue: [
				^ aCriticBlock cull: (self critiqueFor: aMethod) ] ] ]