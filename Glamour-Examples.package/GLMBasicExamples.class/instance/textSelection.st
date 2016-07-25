text
textSelection
	<glmBrowser: 'Text selection' input: '(1 to: 100)'>
	"	|browser |
		browser := self new textSelection.
		browser openOn: (1 to: 100).
		((browser paneNamed: #start) port: #selection) value: 2.
		((browser paneNamed: #end) port: #selection) value: 5.		
	"
	| browser |
	browser := GLMTabulator new.
	browser row: [ :r | r column: #start; column: #end ]; row: #text.
	browser transmit to: #start; andShow: [ :a | a list title: 'Start' ].
	browser transmit to: #end; andShow: [ :a | a list title: 'End' ].
	browser transmit to: #text; andShow: [ :a | 
		a text 
			title: 'Text with selection';
			display: 'some piece of text to test the selection interval on.'].
	browser transmit to: #text->#selectionInterval; from: #start; from: #end; when: [:s :e |  s notNil and: [ e notNil ]] ; transformed: [ :s :e | s to: e ].
"	browser transmit to: #start->#selection; from: #text->#selectionInterval; transformed: [:interval | interval first].
	browser transmit to: #end->#selection; from: #text->#selectionInterval; transformed: [:interval | interval last].
"	^ browser