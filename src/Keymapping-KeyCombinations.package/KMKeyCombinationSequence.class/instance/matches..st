matching
matches: anEventBuffer
	|maxIndex|
	maxIndex := anEventBuffer size min: shortcuts size.
	1 to: maxIndex do: [:index|
		((shortcuts at: index) = (anEventBuffer at: index) asKeyCombination)
			ifFalse: [ ^ false]].
	^ true.