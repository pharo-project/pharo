comparing
= other
	^ other species = self species and: [ 
		other localName = self localName and: [
		other globalName = self globalName ] ]