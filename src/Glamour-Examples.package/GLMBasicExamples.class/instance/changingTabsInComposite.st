composite
changingTabsInComposite
	"self new changingTabsInComposite openOn: '42'"
	<glmBrowser: 'Changing tabs' input: '42'>	
	|browser|
	
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | a 
			title: 'Composite';
			act: [ :aComposite |
				aComposite pane lastActivePresentation: aComposite first] 
			  icon: '1' asMorph imageForm
			  entitled: 'Select first';
			act: [ :aComposite |
				aComposite pane lastActivePresentation: aComposite second] 
			  icon: '2' asMorph imageForm
			  entitled: 'Select second';
			act: [ :aComposite |
				aComposite pane lastActivePresentation: aComposite presentations third] 
			  icon: '3' asMorph imageForm
			  entitled: 'Select third'.
		a list title: 'One'.
		a text title: 'Two'.
		a text title: 'Three'. ].
	^ browser