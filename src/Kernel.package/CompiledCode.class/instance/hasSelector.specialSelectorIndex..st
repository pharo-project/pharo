literals
hasSelector: selector specialSelectorIndex: specialOrNil
	"Answers true if the method refers to the selector.
	 If you don't know what's a special selector, use #hasSelector:.
	 If you do, you may call this method directly to avoid recomputing 
	 the special selector index all the time."
	 (self hasLiteralThorough: selector) ifTrue: [ ^ true ].
	^ specialOrNil 
		ifNil: [ false ] 
		ifNotNil: [ self scanFor: self encoderClass firstSpecialSelectorByte + specialOrNil ]