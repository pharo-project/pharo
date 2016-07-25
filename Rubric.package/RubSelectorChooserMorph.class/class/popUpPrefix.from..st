opening
popUpPrefix: aPrefix from: aTextMorph 
	Cursor wait
		showWhile: [| selectors chooser |
			selectors := self selectorsFromPrefix: aPrefix.
			selectors size = 1
				ifTrue: [aTextMorph editor selectorChooserChosenSelector: selectors first.
					^ nil].
			selectors isEmpty
				ifFalse: [chooser := self new
								chooseBlock: [:chosen | aTextMorph editor selectorChooserChosenSelector: chosen];
								 requestor: aTextMorph;
								 prefix: aPrefix; 
								 labels: selectors;
								 yourself.
					chooser open].
			^ chooser]