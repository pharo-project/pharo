converting
asStringWithCr
	"Convert to a string with returns between items.  Elements are
usually strings.
	 Useful for labels for PopUpMenus."
	| labelStream |
	labelStream := (String new: 200) writeStream.
	self do: [:each |
		each isString
			ifTrue: [labelStream nextPutAll: each; cr]
			ifFalse: [each printOn: labelStream. labelStream cr]].
	self size > 0 ifTrue: [labelStream skip: -1].
	^ labelStream contents