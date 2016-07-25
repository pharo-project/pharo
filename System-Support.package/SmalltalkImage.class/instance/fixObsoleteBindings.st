housekeeping
fixObsoleteBindings
	CompiledMethod allInstances do: [ :method |
		| obsoleteBindings |
		obsoleteBindings := method literals select: [ :literal |
				literal isVariableBinding and: [
					literal value isBehavior and: [ 
						literal value isObsolete ] ] ].
		obsoleteBindings do: [ :binding |
			| obsName realName realClass |
			obsName := binding value name.
			self 
				trace: 'Binding: ';
				traceCr: obsName.
			realName := obsName copyReplaceAll: 'AnObsolete' with: ''.
			realClass := Smalltalk globals at: realName asSymbol ifAbsent: [ UndefinedObject ].
			binding key: binding key value: realClass ].
		"do not forget to flush the JIT cache"
		obsoleteBindings ifNotEmpty: [ method flushCache ] ]