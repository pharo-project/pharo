inline messages
emitTimesRepeat: aMessageNode 
	| limit block limitEmit |
	
	limit := aMessageNode receiver.
	block := aMessageNode arguments last.
	
	limitEmit := [valueTranslator visitNode: limit].
	limit isLiteral | limit isSelf | limit isSuper ifFalse: [
		valueTranslator visitNode: limit.
		methodBuilder addTemp: #limit.
		methodBuilder storeTemp: #limit.
		methodBuilder popTop.
		limitEmit := [methodBuilder pushTemp: #limit]].

	"push start. allocate and initialize iterator"
	self isValueTranslator ifTrue: [ limitEmit value ].
	methodBuilder pushLiteral: 1.
	methodBuilder addTemp: #iterator.
	methodBuilder storeTemp: #iterator.
	methodBuilder popTop. 
	methodBuilder jumpBackTarget: #start.
	methodBuilder pushTemp: #iterator.
	limitEmit value.
	methodBuilder send: #<=.
	methodBuilder jumpAheadTo: #done if: false.

	effectTranslator visitInlinedBlockNode: block.
	methodBuilder pushTemp: #iterator.
	methodBuilder pushLiteral: 1.
	methodBuilder send: #+.
	methodBuilder storeTemp: #iterator.
	methodBuilder popTop.
	methodBuilder jumpBackTo: #start.
	methodBuilder jumpAheadTarget: #done.