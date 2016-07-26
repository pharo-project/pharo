necompletion-extensions
guessTypeForName: aString
	"Packaging notes:
	Since this method is necessary for Ecompletion to work we had the following possibilities:
		- Case 1: add this method as an extension of NECompletion, but it was creating a dependency from NECompletion to 	 		  Rubric.
		- Case 2: add this method in this package. 
		It implies somehow that Rubric depends on NEC but this is a lose coupling, when NECompletion is not loaded, we will get 		some dead code.
		- Case 3: we could create a package NEC-Rubric that depends on Rubric and NECompletion. It seems a bit overkill.
		
	Now it looks like RubSmalltalkEditor could be packaged out of Rubric-Core.
	and it would handle this. 
	In addition the logic completion based on codeCompletionAround:  textMorph:  keyStroke: 
	and the use of the registration as a tool is not really good because the completion requirement is not explicit. 
	A better solution would be that the SmalltalkEditor has a default dummy ecompleter and that we can pass a more 
	advanced one."
		
	^ self interactionModel ifNotNil: [ :im | im guessTypeForName: aString ]