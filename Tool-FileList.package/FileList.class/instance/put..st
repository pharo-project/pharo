private
put: aText
	"Private - put the supplied text onto the file"

	|  type |
	brevityState == #fullFile 
		ifTrue:[
			Cursor write showWhile: [reference writeStream truncate;nextPutAll: aText asString; close].
			^ true].

	listIndex = 0 
		ifTrue: [
			self inform: 'No fileName is selected' translated.
			^ false  "failed" ].
	type := 'These'.
	
	brevityState = #briefFile ifTrue: [type := 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type := 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type := 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type := 'Directory'].
	
	self inform: ('{1} contents cannot
meaningfully be saved at present.' translated format:{type translated}).
	^ false  "failed"