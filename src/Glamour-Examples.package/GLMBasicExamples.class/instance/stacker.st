others
stacker
	<glmBrowser: 'Stacker' input: '5'>
	"self new stacker openOn: 5"
	
	| browser |
	browser := GLMStacker new.
	browser aPane: #x; aPane: #y.
	browser transmit to: #x; andShow: [ :a | 
		a title: 'x'.
		a list display: [:x | 1 to: x ]].
	browser transmit to: #y; andShow: [:a | 
		a title: '10 * x'.
		a list display: [:x |  1 to: (10*x )] ].
	^ browser 