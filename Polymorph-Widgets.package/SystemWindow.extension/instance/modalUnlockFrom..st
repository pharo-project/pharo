*Polymorph-Widgets
modalUnlockFrom: aSystemWindow
	"Unlock the receiver as a modal owner of the given window."

	aSystemWindow removeProperty: #modalOwner.
	self removeProperty: #modalChild.
	closeBox ifNotNil: [:cl | cl enabled: (self valueOfProperty: #preModalCloseEnabled ifAbsent: [true])].
	self removeProperty: #preModalCloseEnabled.
	self activate