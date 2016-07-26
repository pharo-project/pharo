transmitting
addRawSelectionTransmissionTo: aPortSymbol
	^ rawSelectionTransmissions
		add:
			((GLMTransmission new)
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								name: #rawSelection;
								presentation: self;
								yourself);
				destination:
						((GLMPresentationBoundPort new)
								name: aPortSymbol;
								presentation: self;
								yourself))