enumerating
select: selectBlock 
	"Optimized version of Collection>>#select: "

	| newCollection element |
	
	newCollection := self copyEmpty.
	
	firstIndex to: lastIndex do: [ :index |
		(selectBlock value: (element := array at: index))
			ifTrue: [ newCollection addLast: element ]].
		
	^ newCollection