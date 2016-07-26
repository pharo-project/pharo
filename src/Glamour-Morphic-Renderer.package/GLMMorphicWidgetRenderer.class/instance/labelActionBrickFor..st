private
labelActionBrickFor: aPresentation
	
	 ^ aPresentation titleAction 
		ifNil: [ nil ]
		ifNotNil: [ :labelAction |
			((labelAction isAvailableOn: aPresentation) and: [ labelAction hasIcon ])
				ifTrue: [
					GLMActionButtonBrick new
						icon: labelAction icon;
						setBalloonText: labelAction title;
						action: [ labelAction actOn: aPresentation ] ]
				ifFalse: [ nil ] ]
	