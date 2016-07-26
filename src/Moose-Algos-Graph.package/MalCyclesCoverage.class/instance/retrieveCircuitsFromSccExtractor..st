accessing
retrieveCircuitsFromSccExtractor: sccExtractor

	sccExtractor circuits do: [:c |
		circuits add: (c collect: [:n| n model]) ]