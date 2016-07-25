private
readContentsHex: brevity
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData s |

	self reference streamWritable: false do: [ :f| 
		f isNil 
			ifTrue: [^ 'For some reason, this file cannot be read' translated].
		f binary.
		((size := f size)) > 5000 & brevity
			ifTrue: [data := f next: 10000. f close. brevityState := #briefHex]
			ifFalse: [data := f contentsOfEntireFile. brevityState := #fullHex].

		s := (String new: data size*4) writeStream.
		0 to: data size-1 by: 16 do:
			[:loc | s nextPutAll: loc printStringHex; space;
				nextPut: $(; print: loc; nextPut: $); space; tab.
			loc+1 to: (loc+16 min: data size) do: [:i | s nextPutAll: (data at: i) printStringHex; space].
			s cr].
		hexData := s contents].
	
	^ contents := ((size > 5000) & brevity
		ifTrue: ['{3}
------------------------------------------
... end of the first 5000 characters.' translated format: {hexData}]
		ifFalse: [hexData]).
