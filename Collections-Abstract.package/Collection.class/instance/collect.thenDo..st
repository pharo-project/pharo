enumerating
collect: collectBlock thenDo: doBlock 
	"Utility method to improve readability."
	
	^ self do: [ :each|
		doBlock value: (collectBlock value: each)]