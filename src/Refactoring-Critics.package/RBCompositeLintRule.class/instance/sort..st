adding/removing
sort: aBlock
	
	rules first isComposite
		ifTrue: [ rules do: [ :rule | rule sort: aBlock ]] 
		ifFalse: [ self rules: (rules sort: aBlock) ]