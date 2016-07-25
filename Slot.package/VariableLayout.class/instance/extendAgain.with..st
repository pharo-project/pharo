reshaping
extendAgain: aLayout with: someScope
	| result |
	result := self species new.
	result host: aLayout host.
	result slotScope: someScope.
	^ result