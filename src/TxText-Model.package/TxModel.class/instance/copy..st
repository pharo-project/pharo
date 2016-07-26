copying
copy: interval
	"Answer a new text, holding a copy of text enclosed by receiver"

	| copyStartSpan copyEndSpan  newText |
	 
	newText := self class new. 

	interval isEmpty ifTrue: [ ^ newText ].
	
	interval minMaxDo: [ :min :max |
		| span |
		
		span := min span.
		
		[ | copy | 
			copy := span appendCopyTo: newText.
			span == min span ifTrue: [ copyStartSpan := copy ].
			span == max span ifTrue: [ copyEndSpan := copy  ].
			span ~= max span ] whileTrue: [ span := span next ].
		 
		"we cut the end first, to not invalidate the start position"
		((copyEndSpan positionAt: max spanPosition) selectTo: newText endPosition) cut.
		(newText startPosition selectTo: (copyStartSpan positionAt: min spanPosition)) cut.
	 ].

	^ newText

