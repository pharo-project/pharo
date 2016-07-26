others
simpleExpander
	<glmBrowser: 'Simple Expander' input: '#(a b c)'>
	"self new simpleExpander  openOn: #(a b c)"
	| browser |
	browser := GLMExpander new.
	browser show: [ :a | 
		a title: [:x | x ].
		a text title: 'text1'. 
		a text title: 'text2'.].
	^ browser