enumerating
difference: aCollection
	"Answer the set theoretic difference of two collections."

	"#(a b c d e f) difference:  #(a b z k)
	=> #(#f #d #e #c)
	
	#(a b z k) difference: #(a b c d e f) 
	=> #(#k #z) 
	"

	| set |
	set := self asSet.
	aCollection do: [ :each | set remove: each ifAbsent: [  ] ].
	^ self species withAll: set asArray