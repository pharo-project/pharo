protocol
linkRadioButton: aButton
	buttons
		do: [ :b | 
			aButton whenActivatedDo: [ b privateSetState: false ].
			b whenActivatedDo: [ aButton privateSetState: false ] ].
	aButton whenActivatedDo: [ self currentActivated: aButton ].