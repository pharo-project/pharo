copying
postCopy
	"Must copy the associations, or later store will affect both the
		original and the copy.
	Copy any subtrees too!"

	array := array collect: [:assoc |
			assoc ifNil: [nil]
				ifNotNil: [Association
							key: assoc key
							value: ((assoc value isKindOf: KeyedTree)
									ifTrue: [assoc value copy]
									ifFalse: [assoc value])]]