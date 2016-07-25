*renraku
check: aMethod forCritiquesDo: aCriticBlock

	aMethod ast allChildren 
		select: [ :node |
			node isVariable and: [
			node isUndeclared and: [
			node name first isUppercase ]]]
		thenDo: [ :node | aCriticBlock cull:
			(ReTrivialCritique
			withAnchor: (ReSearchStringSourceAnchor
				entity: aMethod 
				string: node name)
			by: self
			hint: node name) ]