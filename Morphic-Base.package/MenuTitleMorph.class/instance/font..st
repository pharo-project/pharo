construction
font: aFont

	titleContainer ifNil: [ ^self ]. "should really not happen!"
	
	titleContainer submorphsDo: [ :stringMorph | stringMorph font: aFont ].