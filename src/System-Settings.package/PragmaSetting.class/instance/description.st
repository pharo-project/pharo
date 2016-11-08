accessing
description
	"Answer the value of description"

	^ description 
		ifNil: [description := ''] 
		ifNotNil: [ description translated ]
