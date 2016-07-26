*Glamour-FastTable
tagBlockWith: num
	| tagBlock |
	tagBlock := [ :item | 
	item isNumber
		ifTrue:
			[ {(item even
				ifTrue: [ 'even' ]
				ifFalse: [ 'odd' ]).
			(item < num
				ifTrue: [ '<' , num asString ]
				ifFalse: [ '>=' , num asString ])} ] ].
	^ tagBlock