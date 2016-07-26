accessing
setChooserWith: aToken labels: labels
	chooser 
		ifNil: [chooser := IdentifierChooserMorph 
							labels: labels
							chooseBlock: [:token | self choose: token]].
	^ chooser