private
basicCommonPrefix
	"Answers the minimum common denominator on package names contained in the monticello package. 
	 It can answer a package in the form X-Y-, with a minus at end..."
	| stream  |
	categories isEmpty ifTrue: [ ^ '' ].
	
	stream := String new writeStream.
	categories first withIndexDo: [:c :index |
		categories do: [:each |
			(each at: index ifAbsent: []) = c 
				ifFalse: [ ^ stream contents ] ].
		stream nextPut: c ].
	
	^stream contents