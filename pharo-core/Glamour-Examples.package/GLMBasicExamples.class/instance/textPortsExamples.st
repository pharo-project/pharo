text
textPortsExamples
	<glmBrowser: 'Text ports' input: '''Type and select to see the results previewed.'''>
	"self new textPortsExamples openOn: 'Type and select to see the results previewed.'"

	| browser |
	browser := GLMTabulator new.
	browser
		row: #main;
		row: [ :r | r column: #selectionInterval; column: #selectedText; column: #full ].
	(browser transmit)
		to: #main;
		andShow: [ :a | a text ].
	(browser transmit)
		to: #selectionInterval;
		from: #main port: #selectionInterval;
		andShow: [ :a | a text title: '#selectionInterval port' ].
	(browser transmit)
		to: #selectedText;
		from: #main port: #selectedText;
		andShow: [ :a | a text title: '#selectedText port' ].
	(browser transmit)
		to: #full;
		from: #main port: #text;
		andShow: [ :a | a text title: '#text port'].
	^ browser