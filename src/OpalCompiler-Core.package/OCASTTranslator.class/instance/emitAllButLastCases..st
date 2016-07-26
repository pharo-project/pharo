inline messages factored
emitAllButLastCases: cases

	|  assocMessageNode  |
	
	1 to: cases size - 1 do: [:i |
		methodBuilder pushDup.
		assocMessageNode := cases at: i.
		valueTranslator visitInlinedBlockNode: assocMessageNode receiver.
		methodBuilder send: #=.
		methodBuilder jumpAheadTo: #next if: false.
		methodBuilder popTop.
		self visitInlinedBlockNode: assocMessageNode arguments first.
		methodBuilder jumpAheadTo: #end.
		methodBuilder jumpAheadTarget: #next.
	]