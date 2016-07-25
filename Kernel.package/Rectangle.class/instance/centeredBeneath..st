transforming
centeredBeneath: aRectangle
	 "Move the receiver so that its top center point coincides with the bottom center point of aRectangle."

	^ self align: self topCenter with: aRectangle bottomCenter