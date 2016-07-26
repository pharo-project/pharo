others
diff
	<glmBrowser: 'Diff' input: '#(#(abc def ghi) #(abc xyz))'>
	"self new diff openOn: #(#(abc def ghi) #(abc xyz))"
	| browser | 
	browser := GLMTabulator new.
	browser 
		row: [:r | r column: #one; column: #two];
		row: #diff.
	browser transmit to: #one; andShow: [ :a |
		a list 
			display: #first ].
	browser transmit to: #two; andShow: [ :a | 
		a list
			display: #second ].
	browser transmit to: #diff; from: #one; from: #two; andShow: [ :a | 
		a diff
			display: [ :one :two | {one asString . two asString}] ].
	^ browser