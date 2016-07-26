reshaping
extendAgain: aLayout with: someScope
	| result |
	result := aLayout copy.
	result slotScope: someScope.
	^ result