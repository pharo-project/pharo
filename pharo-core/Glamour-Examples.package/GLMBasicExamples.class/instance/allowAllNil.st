others
allowAllNil
	<glmBrowser: 'Allowing all nil' input: '5'>
	"self new allowAllNil openOn: 5"
	| browser |
	 
	browser := GLMTabulator new.
	browser 
		column: [: c | c row: #one; row: #two];
		column: #details.
	browser transmit to: #one; andShow: [ :a |
		a tree 
			display:[ :x| 1 to: x ];
			allowDeselection ].
	browser transmit to: #two; andShow: [ :a | 
		a tree 
			display: #(a b c);
			allowDeselection].
	browser transmit to: #details; from: #one; from: #two; andShow: [ :a |
		a text 
			display: [ :one :two | 'At least one is nil: ', one asString, '-', two asString];
			allowNil.
		a text 
			display: [ :one :two | 'All are nil: ', one asString, '-',  two asString];
			allowAllNil ].
		
	"if you need initial display"
	browser transmit to: #details; andShow: [:a | 
		a text display: [:x | 'All are nil'  ] ].
	
	^ browser