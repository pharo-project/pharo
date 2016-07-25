ui requests
edit: aText label: labelString accept: aBlockOrNil
	"Open an editor on the given string/text"
	
	| window pane |
	window := (SystemWindow labelled: labelString) model: self.
	pane := RubScrolledTextMorph new 
				setText: aText asString;
				yourself.
	aBlockOrNil ifNotNil: [  
		pane announcer when: RubTextAcceptRequest do: [:aRequest | aBlockOrNil value: aRequest morph text]].	
	window addMorph: pane frame: (0@0 corner: 1@1); openInWorld.
	^ window
		