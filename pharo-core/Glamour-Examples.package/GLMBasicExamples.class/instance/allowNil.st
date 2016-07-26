others
allowNil
	<glmBrowser: 'Allowing nil' input: '42'>
	"self new allowNil openOn: 42"
	| browser | 
	browser := GLMTabulator new.
	browser 
		column: [: c | c row: #one; row: #two];
		column: #details.
	browser transmit to: #one; andShow: [ :a |
		a tree 
			display: #(1 2 3);
			allowDeselection ].
	browser transmit to: #two; andShow: [ :a | 
		a tree 
			display: #(a b c);
			allowDeselection].
	browser transmit to: #details; from: #one; from: #two; andShow: [ :a | 
		a text 
			display: [ :one :two | one asString, two asString];
			allowNil ].
	^ browser