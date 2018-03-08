Extension { #name : #Duration }

{ #category : #'*Kernel-Chronology-Extras' }
Duration class >> month: aMonth [
	"aMonth is an Integer or a String"
	
	^ (Month month: aMonth) duration

]
