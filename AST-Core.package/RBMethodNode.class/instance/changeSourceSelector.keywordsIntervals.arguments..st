private-replacing
changeSourceSelector: oldSelector keywordsIntervals: oldIntervals arguments: oldArguments
	"If this is the same number of arguments, we try a one to one replacement of selector parts and arguments. If this is not the c
ase try to rewrite the signature as a whole, what unfortunately drops the comments within the signature."

	[ (oldIntervals size = self keywordsPositions size and: [ oldArguments size = arguments size ])
		ifTrue: [
			self selectorParts keysAndValuesDo: [ :index :part |
				self addReplacement: (RBStringReplacement 
					replaceFrom: (oldIntervals at: index) first
					to: (oldIntervals at: index) last
					with: part) ].
			oldArguments with: arguments do: [ :old :new | 
				self addReplacement: (RBStringReplacement 
					replaceFrom: old start
					to: old stop
					with: new name) ] ]
		ifFalse: [
			self addReplacement: (RBStringReplacement
				replaceFrom: oldIntervals first first
				to: (oldArguments notEmpty
					ifTrue: [ oldArguments last stop ]
					ifFalse: [ oldIntervals last last ])
				with: (String streamContents: [ :stream |
					self selectorParts keysAndValuesDo: [ :index :part |
						index = 1 ifFalse: [ stream space ].
						stream nextPutAll: part.
						index <= arguments size
							ifTrue: [ stream space; nextPutAll: (arguments at: index) name ] ] ])) ] ]
		on: Error
		do: [ :ex | ex return ]