comparing
= aTempVar

	^aTempVar class = self class 
		and: [aTempVar scope = self scope 
		and: [aTempVar name = self name
		and: [aTempVar usage = self usage]]].

	