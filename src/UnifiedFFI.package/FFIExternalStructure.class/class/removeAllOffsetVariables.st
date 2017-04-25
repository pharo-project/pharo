private
removeAllOffsetVariables
	self classPool keys 
		select: [ :each | each beginsWith: 'OFFSET_' ]
		thenDo: [ :each | self removeClassVarNamed: each ]