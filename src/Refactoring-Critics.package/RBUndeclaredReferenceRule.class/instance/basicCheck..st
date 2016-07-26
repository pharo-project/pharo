running
basicCheck: aMethod 

	^ aMethod ast allChildren 
		anySatisfy: [ :node |
			node isVariable and: [
			node isUndeclared and: [
			node name first isUppercase ]]]