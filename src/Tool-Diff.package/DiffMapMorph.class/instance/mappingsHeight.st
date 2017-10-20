private - accessing
mappingsHeight
	"Answer the maximum y of all the mappings."
	
	self mappings ifEmpty: [^0].
	^self mappings last dst range last 