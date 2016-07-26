private
childGeneratorBlock: doBlock matching: patterns
	"
	FileSystem workingDirectory filesMatching: '*'
	FileSystem workingDirectory filesMatching: '*.image;*.changes'
	"
	
	| files reference|
	files := Set new.
	reference := self resolve.
	
	(patterns findTokens: ';', String crlf) do: [ :pattern | 
		doBlock value: reference value: [ :basename|
			(pattern match: basename)
				ifTrue: [ files add: (self / basename) ]]].
	
	^files asOrderedCollection