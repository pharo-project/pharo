private
readContentsBrief: brevityFlag
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first5000 |
	self reference binaryReadStreamDo: [:stream |	
		(brevityFlag not or: [(fileSize := stream size) <= 100000]) 
			ifTrue:[
				contents :=   self streamFullContentsFrom: stream.
				brevityState := #fullFile.   "don't change till actually read"
				^ contents].

		"if brevityFlag is true, don't display long files when first selected"
		 first5000 := self streamFirst5000BytesFrom: stream].
	
	contents := '{1}
------------------------------------------
... end of the first 5000 characters.' translated format: {first5000}.
	brevityState := #briefFile.   "don't change till actually read"
	^ contents.
