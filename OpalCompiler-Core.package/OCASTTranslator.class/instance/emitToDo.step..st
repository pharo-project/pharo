inline messages factored
emitToDo: aMessageNode step: step
	| limit block iterator limitEmit |
	
	limit := aMessageNode arguments first.
	block := aMessageNode arguments last.
	iterator := block arguments first binding.
	
	limitEmit := [valueTranslator visitNode: limit].
	limit isLiteralNode | limit isSelf | limit isSuper | limit isArgument ifFalse: [
		valueTranslator visitNode: limit.
		methodBuilder addTemp: (iterator name, #limit).
		methodBuilder storeTemp: (iterator name, #limit).
		methodBuilder popTop.
		limitEmit := [methodBuilder pushTemp: (iterator name, #limit)]].

	"push start. allocate and initialize iterator"
	valueTranslator visitNode: aMessageNode receiver.
	iterator emitStore: methodBuilder.	
	self isEffectTranslator ifTrue: [methodBuilder popTop]. 
	methodBuilder jumpBackTarget: #start.
	iterator emitValue: methodBuilder.	
	limitEmit value.
	methodBuilder send: (step > 0 ifTrue: [#<=] ifFalse: [#>=]).
	methodBuilder jumpAheadTo: #done if: false.

	effectTranslator visitInlinedBlockNode: block.
	iterator emitValue: methodBuilder.
	methodBuilder pushLiteral: step.
	methodBuilder send: #+.
	iterator emitStore: methodBuilder.
	methodBuilder popTop.
	methodBuilder jumpBackTo: #start.
	methodBuilder jumpAheadTarget: #done.