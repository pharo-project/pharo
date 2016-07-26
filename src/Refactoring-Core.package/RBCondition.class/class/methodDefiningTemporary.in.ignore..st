utilities
methodDefiningTemporary: aString in: aClass ignore: aBlock 
	| searcher method |
	searcher := RBParseTreeSearcher new.
	method := nil.	"Shut-up the warning"
	searcher matches: aString do: [:aNode :answer | ^method].
	aClass withAllSubclasses do: 
			[:class | 
			class selectors do: 
					[:each | 
					(aBlock value: class value: each) 
						ifFalse: 
							[| parseTree |
							method := class methodFor: each.
							parseTree := class parseTreeFor: each.
							parseTree notNil ifTrue: [searcher executeTree: parseTree]]]].
	^nil