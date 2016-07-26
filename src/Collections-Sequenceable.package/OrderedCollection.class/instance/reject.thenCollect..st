enumerating
reject: rejectBlock thenCollect: collectBlock
    " Optimized version of Collection>>#reject:thenCollect: "

	| newCollection |

    newCollection := self copyEmpty.

    firstIndex to: lastIndex do: [ :index |
		| element |
		element := array at: index.
		(rejectBlock value: element) 
			ifFalse: [ newCollection addLast: (collectBlock value: element) ]].

    ^ newCollection