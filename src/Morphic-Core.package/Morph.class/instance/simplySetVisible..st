accessing
simplySetVisible: aBoolean
	"Set the receiver's visibility property.  This mild circumlocution is because my TransfomationMorph #visible: method would also set the visibility flag of my flexee, which in this case is pointless because it's the flexee that calls this.
	This appears in morph as a backstop for morphs that don't inherit from TFMorph"

	self visible: aBoolean