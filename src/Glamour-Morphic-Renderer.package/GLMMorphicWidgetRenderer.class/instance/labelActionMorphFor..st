private
labelActionMorphFor: aPresentation
	
	 ^ aPresentation titleAction 
		ifNil: [ nil ]
		ifNotNil: [ :labelAction |
			((labelAction isAvailableOn: aPresentation) and: [ labelAction hasIcon ])
				ifTrue: [ 
					MultistateButtonMorph new
						extent: labelAction icon extent;
						activeEnabledOverUpFillStyle: (ImageFillStyle form: labelAction icon);
						setBalloonText: labelAction title;
						addUpAction: [ labelAction actOn: aPresentation ] ]
				ifFalse: [ nil ] ]
	