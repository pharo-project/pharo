examples
exampleDragAndDrop4
	"Show drag&drop in one list, with autoscroll"
	<example>
	| list |

	list := FTTableMorph new
		extent: 300@300;
		addColumn: (FTColumn id: 'Drag from & to me!');
		dataSource: (FTExampleDropDataSource elements: (Smalltalk allClassesAndTraits sorted: [ :a :b | a name < b name ]));
		dragEnabled: true;
		dropEnabled: true;
		yourself.
		
	^ list openInWindowLabeled: 'Autoscrolling'