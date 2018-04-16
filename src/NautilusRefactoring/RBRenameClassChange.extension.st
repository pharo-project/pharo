Extension { #name : #RBRenameClassChange }

{ #category : #'*NautilusRefactoring' }
RBRenameClassChange >> nameToDisplay [

	^ self printString
]

{ #category : #'*NautilusRefactoring' }
RBRenameClassChange >> oldVersionTextToDisplay [
	^ String streamContents: [ :s | 
			self changes do: [ :c | 	s << c oldVersionTextToDisplay; newLine ]]

]

{ #category : #'*NautilusRefactoring' }
RBRenameClassChange >> textToDisplay [
	^ String
		streamContents: [ :s | 
			s
				<< self printString;
				newLine.
			self changes
				do: [ :c | 
					s
						<< c textToDisplay;
						newLine ] ]
]

{ #category : #'*NautilusRefactoring' }
RBRenameClassChange >> whatToDisplayIn: aChangeBrowser [
	^ { self }
	
]
