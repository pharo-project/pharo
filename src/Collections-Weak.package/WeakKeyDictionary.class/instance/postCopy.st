copying
postCopy
	"Must copy the associations, or later store will affect both the
original and the copy"

	array := array collect: [:assoc |
				assoc 
					ifNil: [nil]
					ifNotNil: [
						assoc expired 
							ifTrue: [ WeakKeyAssociation expired]
							ifFalse: [WeakKeyAssociation key: assoc key value: assoc value]]]