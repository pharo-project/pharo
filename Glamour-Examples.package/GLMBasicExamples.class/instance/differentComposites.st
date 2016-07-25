composite
differentComposites
	"self new differentComposites openOn: (1 to: 100)"
	<glmBrowser: 'Composite arrangements' input: '(1 to: 100)'>	
	| browser |
	browser := GLMTabulator new.
	browser row: [:r | 
		r column: #tabbed; column: #accordion; column: #stackedVertically] span: 4; row: #preview.
	browser transmit to: #tabbed; andShow: [:a | 
		a title: 'Tabs'.
		a list title: 'List'. a text title: 'Text' ].
	browser transmit to: #accordion; andShow: [:a | 
		a accordionArrangement.
		a title: 'Accordion'.
		a list title: 'List'. a text title: 'Text' ].
	browser transmit to: #stackedVertically; andShow: [:a | 
		a title: 'Stack'.
		a stackedVerticallyArrangement.
		a list title: 'List'. a text title: 'Text' ].
	browser transmit to: #preview; from: #tabbed; andShow: [:a | 
		a text display: [:x | x printString, ' from tabbed' ]].
	browser transmit to: #preview; from: #accordion; andShow: [:a | 
		a text display: [:x | x printString, ' from accordion' ]].
	browser transmit to: #preview; from: #stackedVertically; andShow: [:a | 
		a text display: [:x | x printString, ' from stacked vertically' ]].
	^ browser 