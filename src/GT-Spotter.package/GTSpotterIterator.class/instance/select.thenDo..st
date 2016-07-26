mimic various enumerating
select: aBlock thenDo: aBlock2
	self do: [ :each | 
		(aBlock value: each) 
			ifTrue: [ aBlock2 value: each ] ]