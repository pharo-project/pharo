Extension { #name : 'Package' }

{ #category : '*Extension-New-System' }
Package >> description [

	| stream |
	stream := String new writeStream.

	stream
		nextPutAll: 'Package << #';
		nextPutAll: self name;
		cr;
		tab;
		nextPutAll: 'imports: { '.

	self importedExtensions ifNotEmpty: [ :imports |
		imports do: [ :i | stream nextPutAll: '#' , i name asString ] separatedBy: [ stream nextPutAll: ' . ' ] ].

	stream nextPutAll: ' }'.

	^ stream contents
]
