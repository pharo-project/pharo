actions
saveVersion
	self canSave ifFalse: [ ^ self ].
	"fork the whole version creation "
	[ self basicSaveVersionIn: self repository ] fork