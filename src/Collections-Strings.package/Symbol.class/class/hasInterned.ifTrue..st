private
hasInterned: aString ifTrue: symBlock
	"Answer with false if aString hasnt been interned (into a Symbol),  
	otherwise supply the symbol to symBlock and return true."

	^ (self lookup: aString)
		ifNil: [ false ]
		ifNotNil: [ :symbol | 
			symBlock value: symbol.
			true ]