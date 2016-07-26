internal
is: rect saneWithRespectTo: morphs

	^(morphs anySatisfy: [ :morph | morph owner isNotNil and: [morph bounds intersects: rect]]) not
