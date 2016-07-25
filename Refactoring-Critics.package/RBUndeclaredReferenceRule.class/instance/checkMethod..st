running
checkMethod: aMethod 

	aMethod ast allChildren 
		select: [ :node |
			node isVariable and: [
			node isUndeclared and: [
			node name first isUppercase ]]]
		thenDo: [ :node |
			result
				addSearchString: node name;
				addMethod: aMethod ]