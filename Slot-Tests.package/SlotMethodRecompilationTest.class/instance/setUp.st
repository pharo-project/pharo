running
setUp
	class := self make: [ :builder |
		builder traitComposition: TOne ].
	class compile: 'm1'.
	