geometry
horizontalOffsetFor: textPosition 
	
	| totalAdvance |
	
	totalAdvance := 0. 
	
	"special case, when line starting position don't matching the first command position"
	commands isEmpty ifTrue: [ "empty line" ^ 0 ].
	commands first startPosition > textPosition ifTrue: [  ^ 0 ]. 
		
	commands do: [:cmd |
		(cmd contains: textPosition) ifTrue: [
			^ totalAdvance + (cmd horizontalAdvanceUpTo: textPosition layout: layout)
			  ]
		ifFalse: [ totalAdvance := totalAdvance + cmd advance x]
	].

	"if we reach here, just answert he accumulated total advance"
	^ totalAdvance