examples
exampleEasyList2
	"Show an easy list with all Morph methods and a simple menu"
	<example>
	| list |

	list := FTEasyListMorph new
		extent: 300@550;
		elements: Morph methods;
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
