reshaping
reshapeFrom: oldScope to: newParentLayout
	"Answer the reshaped version of me for a new parent layout.

	If the new parent layout has not fields, then the new version 
	is just a copy of me. But if it has fields, that means that 
	the new version of me cannot be a bits layout anymore, so the
	answer is a new default layout. "

	^ newParentLayout hasFields 
		ifTrue: [ 
			(newParentLayout extend)
				host: host;
				yourself ]
		ifFalse: [ 
			self copy ]
