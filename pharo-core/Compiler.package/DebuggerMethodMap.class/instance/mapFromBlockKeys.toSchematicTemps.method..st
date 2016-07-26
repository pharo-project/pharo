private
mapFromBlockKeys: keys toSchematicTemps: schematicTempNamesString method: aCompiledMethod
	"Decode a schematicTempNamesString that encodes the layout of temp names
	 in a method and any closures/blocks within it, matching keys in keys to
	 vectors of temp names."
	| map tempNames |
	map := Dictionary new.
	tempNames := schematicTempNamesString readStream.
	keys do:
		[:key| | tempSequence tempIndex |
		tempSequence := OrderedCollection new.
		tempIndex := 0.
		[(tempNames skipSeparators; peek) ifNil: [true] ifNotNil: [:ch| '[]' includes: ch]] whileFalse:
			[tempNames peek = $(
				ifTrue: [tempSequence addAllLast: ((self tempsSubSequenceFrom: (tempNames next; yourself)) withIndexCollect:
														[:temp :index|
														{ temp. { tempIndex + 1. index } }]).
						tempNames peek ~= $) ifTrue: [self error: 'parse error'].
						tempIndex := tempIndex + 1.
						tempNames next]
				ifFalse: [tempSequence addAllLast: ((self tempsSubSequenceFrom: tempNames) withIndexCollect:
														[:temp :index|
														{ temp. tempIndex := tempIndex + 1 }])]].
		map at: key put: tempSequence asArray.
		[tempNames peek = $]] whileTrue: [tempNames next].
		tempNames peek = $[ ifTrue:
			[tempNames next]].
	^map