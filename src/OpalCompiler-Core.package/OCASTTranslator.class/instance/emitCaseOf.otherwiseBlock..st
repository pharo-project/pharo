inline messages factored
emitCaseOf: aMessageNode otherwiseBlock: aBlock

	| cases assocMessageNode  |
	
	cases := aMessageNode arguments first statements.
	cases ifEmpty: [ self backendError: 'At least one case required' forNode: aMessageNode ].
	valueTranslator visitNode: aMessageNode receiver.
	valueTranslator emitAllButLastCases: cases.
	
	"last case with otherwise"
	assocMessageNode := cases last.
	valueTranslator visitInlinedBlockNode: assocMessageNode receiver.
	methodBuilder send: #=.
	methodBuilder jumpAheadTo: #next if: false.
	valueTranslator visitInlinedBlockNode: assocMessageNode arguments first.
	methodBuilder jumpAheadTo: #end.
	methodBuilder jumpAheadTarget: #next.
	aBlock value.

	cases size timesRepeat: [methodBuilder jumpAheadTarget: #end].
	
	aMessageNode lastIsReturn 
		ifTrue: [
			(aMessageNode owningScope) isBlockScope 
				ifTrue: [methodBuilder blockReturnTop] 
				ifFalse: [methodBuilder returnTop]] 
		ifFalse: [self isEffectTranslator ifTrue: [methodBuilder popTop]].