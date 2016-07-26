examples
exampleEasyList3
	"Show an easy list with all Morph methods and a simple menu, with a list header"
	<example>
	| list |

	list := FTEasyListMorph new
		extent: 300@550;
		elements: Morph methods;
		header: 'Method list';
		icon: [ :method | (FTExampleMethodModel method: method) actionOn: nil ];
		display: [ :method | method selector ];		
		menu: [ :method | 
			MenuMorph new 
				add: ('Browse #', (method methodClass name), '>>#', method selector)
					target: method
					selector: #browse;
				add: ('Inspect #', (method methodClass name), '>>#', method selector)
					target: method
					selector: #inspect;
				yourself ];
		yourself.
		
	^ list openInWindow
