private
presentation: aPresentation isTheLeftmostRecepientIn: aStatement

	^ (aStatement isMessage or:
	 [ aStatement isCascade ])  and: [ 
			aStatement leftmostChainReceiver = aPresentation ]