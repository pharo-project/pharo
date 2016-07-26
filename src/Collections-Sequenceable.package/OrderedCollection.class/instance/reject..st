enumerating
reject: rejectBlock 
	"Optimized version of Collection>>#reject:"

	| newCollection element |
	
	newCollection := self copyEmpty.
	
	firstIndex to: lastIndex do: [ :index |
		(rejectBlock value: (element := array at: index))
			ifFalse: [ newCollection addLast: element ]].
		
	^ newCollection