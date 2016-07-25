*NautilusRefactoring
oldVersionTextToDisplay
	^ String streamContents: [ :s | 
			self changes do: [ :c | 	s << c oldVersionTextToDisplay; cr ]]
