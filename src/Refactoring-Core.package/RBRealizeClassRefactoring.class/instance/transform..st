transforming
transform: aClass
	| class method parseTree |
	aClass allSelectors do: [ :selector |
		class := aClass whoDefinesMethod: selector.
		(class notNil and: [ class ~= aClass ]) ifTrue: [
			method := class methodFor: selector.
			(method notNil and: [ method refersToSymbol: #subclassResponsibility ]) ifTrue: [
				parseTree := method parseTree.
				parseTree body 
					temporaries: OrderedCollection new; 
					statements: OrderedCollection new;
					addNode: (RBMessageNode 
						receiver: (RBVariableNode named: 'self')
						selector: #shouldBeImplemented).
				aClass compile: parseTree newSource withAttributesFrom: method ] ] ]