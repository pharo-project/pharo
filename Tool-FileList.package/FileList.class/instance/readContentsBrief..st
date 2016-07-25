private
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first5000 |

	self reference streamWritable: false do: [:f|		
		(brevityFlag not or: [(fileSize := f size) <= 100000]) 
			ifTrue:[
				contents := [ f contents asString ]
					on: Error
					do: [ :ex | ^ f binary contents asString ].
				brevityState := #fullFile.   "don't change till actually read"
				^ contents].

		"if brevityFlag is true, don't display long files when first selected"
		[ first5000 := f next: 5000 ]
			on: Error
			do: [ :ex | first5000 := (f binary next: 5000) asString  ]].
	
	contents := '{1}
------------------------------------------
... end of the first 5000 characters.' translated format: {first5000}.
	brevityState := #briefFile.   "don't change till actually read"
	^ contents.
