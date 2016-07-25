enumerating
difference: aCollection
	"Answer the set theoretic difference of two collections. Optimized version for Sets where no intermediate Set is necessary"

	"#(a b c d e f) difference:  #(a b z k)
	=> #(#f #d #e #c)
	
	#(a b z k) difference: #(a b c d e f) 
	=> #(#k #z) 
	"

	| set |
	set := self copy.
	aCollection do: [ :each | set remove: each ifAbsent: [  ] ].
	^ set