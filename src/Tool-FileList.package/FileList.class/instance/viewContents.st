own services
viewContents
	"View the contents of my selected file in a new text window"
	
	| aString |
	
	self reference streamWritable: false do: [ :stream|
		aString := stream setConverterForCode contentsOfEntireFile ].
	
	UIManager default edit: aString label: 'Contents from ', self reference basename