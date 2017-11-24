"
In the topics of this book You will find references to the concrete critiques implementations. For more please browse the subclasses of ReAbstractCritique
"
Class {
	#name : #RenrakuCritiqueExamplesHelp,
	#superclass : #RenrakuCritiqueHelp,
	#category : #'Renraku-Help'
}

{ #category : #accessing }
RenrakuCritiqueExamplesHelp class >> bookDescription [

	^ 'In the topics of this book You will find references to the concrete critiques implementations. For more please browse the subclasses of ReAbstractCritique'
]

{ #category : #accessing }
RenrakuCritiqueExamplesHelp class >> bookName [

	^ 'Critique Examples'
]

{ #category : #pages }
RenrakuCritiqueExamplesHelp class >> missingMethod [

	^ self topicForClass: ReMissingMethodCritique
]

{ #category : #accessing }
RenrakuCritiqueExamplesHelp class >> pages [


	^ #(trivial missingMethod transformation replaceNode refactoring)
]

{ #category : #pages }
RenrakuCritiqueExamplesHelp class >> refactoring [

	^ self topicForClass: ReRefactoringCritique  
]

{ #category : #pages }
RenrakuCritiqueExamplesHelp class >> replaceNode [

	^ self topicForClass: ReReplaceNodeCritique 
]

{ #category : #pages }
RenrakuCritiqueExamplesHelp class >> transformation [

	^ self topicForClass: ReTransformationCritique
]

{ #category : #pages }
RenrakuCritiqueExamplesHelp class >> trivial [

	^ self topicForClass: ReTrivialCritique
]
