visitor
visitDoItChunk: aChunk

	| contents |
	contents := aChunk contents.
	('*ubclass:*instanceVariableNames:*classVariableNames:*poolDictionaries:*category:*'
		match: contents) ifTrue:[^self classDefinition: contents with: aChunk].
	('* class*instanceVariableNames:*'
		match: contents) ifTrue:[^self metaClassDefinition: contents with: aChunk].		
	('Trait*named:*uses:*category:*'
		match: contents) ifTrue:[^self traitDefinition: contents with: aChunk].
	('* classTrait*uses:*'
		match: contents) ifTrue:[^self metaTraitDefinition: contents with: aChunk].	
	('* removeSelector:*'
		match: contents) ifTrue:[^self removedMethod: contents with: aChunk].
	('* comment:*'
		match: contents) ifTrue:[^self msgClassComment: contents with: aChunk].
	('* initialize'
		match: contents) ifTrue:[^self]. "Initialization is done based on class>>initialize"
	doIts add: aChunk.