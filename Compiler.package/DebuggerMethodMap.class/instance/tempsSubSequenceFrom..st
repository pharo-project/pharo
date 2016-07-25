private
tempsSubSequenceFrom: tempNamesStream
	^Array streamContents:
		[:tsss|
		[tempNamesStream skipSeparators.
		 tempNamesStream atEnd
		 or: ['[]()' includes: tempNamesStream peek]] whileFalse:
			[tsss nextPut: (String streamContents:
							[:s|
							[s nextPut: tempNamesStream next.
							 tempNamesStream peek
								ifNil: [true]
								ifNotNil: [:peek| ' []()' includes: peek]] whileFalse])]]

	"thisContext method tempsSubSequenceFrom: 'les temps perdu(sont n''est pas la)' readStream"
	"thisContext method tempsSubSequenceFrom: ('les temps perdu(sont n''est pas la)' readStream skipTo: $(; yourself)"