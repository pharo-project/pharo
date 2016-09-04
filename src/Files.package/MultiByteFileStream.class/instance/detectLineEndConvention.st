private - crlf
detectLineEndConvention
	"Detect the line end convention used in this stream. The result may be either #cr, #lf or #crlf."
	| char numRead state |
	self isBinary ifTrue: [^ self error: 'Line end conventions are not used on binary streams'].
	wantsLineEndConversion == true ifFalse: [self lineEndConvention: nil.
					^lineEndConvention].
	self closed ifTrue: [self lineEndConvention: LineEndDefault.
					^lineEndConvention].

	"Default if nothing else found"
	numRead := 0.
	state := self converter saveStateOf: self.
	lineEndConvention := nil.
	[super atEnd not and: [numRead < LookAheadCount]]
		whileTrue: 
			[char := self next.
			char = Lf
				ifTrue: 
					[converter restoreStateOf: self with: state.
					self lineEndConvention: #lf.
					^lineEndConvention].
			char = Cr
				ifTrue: 
					[self peek = Lf
						ifTrue: [self lineEndConvention: #crlf]
						ifFalse: [self lineEndConvention: #cr].
					converter restoreStateOf: self with: state.
					^ lineEndConvention].
			numRead := numRead + 1].
	converter restoreStateOf: self with: state.
	self lineEndConvention: LineEndDefault.
	^ lineEndConvention