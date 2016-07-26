*NautilusRefactoring
whatToDisplayIn: aChangeBrowser

	| result |
	result := OrderedCollection with: self.
	^ result, (self changes flatCollect: [:e | e changes ])