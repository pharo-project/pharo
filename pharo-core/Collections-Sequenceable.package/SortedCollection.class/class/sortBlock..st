instance creation
sortBlock: aBlock 
	"Answer an instance of me such that its elements are sorted according to 
	the criterion specified in aBlock."

	^(super new: 10) sortBlock: aBlock