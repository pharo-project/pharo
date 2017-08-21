instance creation
nextTag
	^(self allSubclasses
		collect: [ :e | 
			(e class canUnderstand: #tag)
				ifTrue: [ e tag ]
				ifFalse: [ -1 ] ]) max + 1