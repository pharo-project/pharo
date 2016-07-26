updating
removeCircularDependenciesFrom: circuitNode
	circuitNode nextNodes removeAllSuchThat: [ :each | each = circuitNode ].
	circuitNode previousNodes removeAllSuchThat: [ :each | each = circuitNode ]