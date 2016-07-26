class hierarchy
subclassesDo: aBlock
	"Evaluate aBlock with all subclasses of nil.  Others are not direct subclasses of Class."

	^ Class subclassesDo: [:cl | 
			cl isMeta ifTrue: [aBlock value: cl soleInstance]].