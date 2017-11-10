Extension { #name : #RBRenameClassChange }

{ #category : #'*NautilusRefactoring' }
RBRenameClassChange >> nameToDisplay [

	^ self printString
]

{ #category : #'*NautilusRefactoring' }
RBRenameClassChange >> oldVersionTextToDisplay [
	^ String streamContents: [ :s | 
			self changes do: [ :c | 	s << c oldVersionTextToDisplay; cr ]]

]

{ #category : #'*NautilusRefactoring' }
RBRenameClassChange >> textToDisplay [
	^ String streamContents: [ :s | 
         s << self printString ; cr.
         self changes do: [ :c | 	s << c textToDisplay; cr ]]

]

{ #category : #'*NautilusRefactoring' }
RBRenameClassChange >> whatToDisplayIn: aChangeBrowser [
	^ { self }
	
]
