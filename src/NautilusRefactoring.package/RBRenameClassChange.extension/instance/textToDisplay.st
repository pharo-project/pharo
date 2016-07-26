*NautilusRefactoring
textToDisplay
	^ String streamContents: [ :s | 
         s << self printString ; cr.
         self changes do: [ :c | 	s << c textToDisplay; cr ]]
