constructor
codeMethod: selector block: block tempVars: vars primitive: primitive class: class

	| blockNode selectorNode visibleTemps invisibleTemps arguments temporaries |
	selectorNode := self codeSelector: selector code: nil.
	tempVars := vars.
	visibleTemps := OrderedCollection new.
	invisibleTemps := OrderedCollection new.
	tempVars do: [:t|
				   ((t isIndirectTempVector or: [t scope >= 0])
						ifTrue: [visibleTemps]
						ifFalse: [invisibleTemps]) addLast: t].
	arguments := visibleTemps copyFrom: 1 to: nArgs.
	temporaries := visibleTemps copyFrom: nArgs + 1 to: visibleTemps size.
	block
		arguments: arguments;
		temporaries: temporaries.
	blockNode := MethodNode new
		selector: selectorNode
		arguments: arguments
		precedence: selector precedence
		temporaries: temporaries
		block: block
		encoder: (LegacyEncoderForV3PlusClosures new initScopeAndLiteralTables
					temps: visibleTemps, invisibleTemps
					literals: literalValues
					class: class)
		primitive: primitive
		properties: method properties copy.
	blockNode properties method: blockNode.
	^blockNode