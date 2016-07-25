accessing
searchField
	^ searchField ifNil: [ 
		searchField := SearchMorph new
			model: self;
			updateSelector: #searchUpdate:;
			on: (Character arrowDown asKeyCombination) do: [ :arg :arg2 :keystroke | 
				self tree spec instance handleKeystroke: keystroke ];
			yourself.
		searchField := searchField asSpecAdapter ]