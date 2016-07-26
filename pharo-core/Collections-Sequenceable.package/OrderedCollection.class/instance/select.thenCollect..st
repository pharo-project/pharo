enumerating
select: selectBlock thenCollect: collectBlock
    " Optimized version Collection>>#select:thenCollect: "

	| newCollection element |

    newCollection := self copyEmpty.

    firstIndex to: lastIndex do: [ :index |
		element := array at: index.
		(selectBlock value: element) 
			ifTrue: [ newCollection addLast: (collectBlock value: element) ]].

    ^ newCollection