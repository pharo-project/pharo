opening
popUpPrefix: aPrefix from: aTextMorph 
	Cursor wait
		showWhile: [| selectors chooser |
			selectors := self selectorsFromPrefix: aPrefix.
			selectors size = 1
				ifTrue: [aTextMorph editor editingState chosenSelector: selectors first for: aTextMorph.
					^ nil].
			selectors isEmpty
				ifFalse: [chooser := self new
								chooseBlock: [:chosen | aTextMorph editor editingState chosenSelector: chosen for: aTextMorph];
								 requestor: aTextMorph;
								 prefix: aPrefix; 
								 labels: selectors;
								 yourself.
					chooser open].
			^ chooser]