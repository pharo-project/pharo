examples
example
	| m |

	m := self new.
	m openWithSpec.
	
	m roots: #(a b c d).
	m childrenBlock: [:it | it = #c 
		ifTrue: [ {1. 2. 3} ]
		ifFalse: [ {} ]].
	m displayBlock: [:item | item isSymbol
		ifTrue: [ item asUppercase , ' symbol' ]
		ifFalse: [ item printString ]].
	m 
		dragEnabled: true;
		dropEnabled: true;
		wantDropBlock: [ :draggedItem :event :source | 
			draggedItem isTransferable and: [ draggedItem source = m ] ];
		acceptDropBlock: [ :transfer :event :source :receiver | 
			Transcript 
				show: 'Dragged node '; 
				show: transfer passenger content; 
				show: ' to node '; 
				show: receiver content; 
				cr ].
		
	^ m