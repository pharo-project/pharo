enumerating
collect: collectBlock thenSelect: selectBlock
    "Optimized version Collection>>#collect:thenSelect:"

    | newCollection newElement |

    newCollection := self copyEmpty.
    firstIndex to: lastIndex do: [ :index | 
		newElement := collectBlock value: (array at: index).
		(selectBlock value: newElement) 
			ifTrue: [ newCollection addLast: newElement ]].
    ^ newCollection