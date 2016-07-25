pragmas
pragmaStatement
	"Read a single pragma statement. Parse all generic pragmas in the form of: <key1: val1 key2: val2 ...> and remember them, including primitives."
	
	| selector arguments words index keyword |
	(hereType = #keyword or: [ hereType = #word or: [ hereType = #binary ] ])
		ifFalse: [  ^ self expected: 'pragma declaration' ].

	" This is a ugly hack into the compiler of the FFI package. FFI should be changed to use propre pragmas that can be parsed with the code here. "
	(here = #apicall: or: [ here = #cdecl: ])
		ifTrue: [ ^ self externalFunctionDeclaration ].

	selector := String new.
	arguments := OrderedCollection new.
	words := OrderedCollection new.
	[ hereType = #keyword or: [ (hereType = #word or: [ hereType = #binary ]) and: [ selector isEmpty ] ] ] whileTrue: [
		index := self startOfNextToken + requestorOffset.
		selector := selector , self advance.
		words add: (index to: self endOfLastToken + requestorOffset).
		(selector last = $: or: [ selector first isLetter not ])
			ifTrue: [ arguments add: (self pragmaLiteral: selector) ] ].
	selector numArgs ~= arguments size
		ifTrue: [ ^ self expected: 'pragma argument' ].
	(Symbol hasInterned: selector 
		ifTrue: [ :value | keyword := value]) 
		ifFalse: [ 
			keyword := self 
				correctSelector: selector wordIntervals: words
				exprInterval: (words first first to: words last last)
				ifAbort: [ ^ self fail ] ].
	self addPragma: (Pragma keyword: keyword arguments: arguments asArray).
	^ true