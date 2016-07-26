examples
exampleEasyList1
	"Show an easy list with all Morph methods"
	<example>
	| list |

	list := FTEasyListMorph new
		extent: 300@550;
		elements: Morph methods;
		icon: [ :method | (FTExampleMethodModel method: method) actionOn: nil ];
		"display: [ :method | method selector ];	"	
		yourself.
		
	^ list openInWindow
