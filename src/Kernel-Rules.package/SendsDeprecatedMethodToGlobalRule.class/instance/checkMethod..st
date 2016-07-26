running
checkMethod: aMethod

	aMethod ast nodesDo: [ :node |
		(node isVariable and: [
			node binding ifNil: [ node doSemanticAnalysis ].
			node isGlobal ]) ifTrue: [ 
			| message |
			message := node parent.
			(message isMessage and: [
			 self check: message selector forDeprecationIn: node name ]) ifTrue: [ 
				result
					addMethod: aMethod;
					addSearchString: message selector
				 ] ] ]