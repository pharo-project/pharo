"
Critique is an object that binds an entity with a rule that is violated by that entity. The critique describes a specific violation, and may provide a solutions to fix it.
"
Class {
	#name : #RenrakuCritiqueHelp,
	#superclass : #RenrakuHelp,
	#category : 'Renraku-Help'
}

{ #category : #accessing }
RenrakuCritiqueHelp class >> bookDescription [
	^ 'Critique is an object that binds an entity with a rule that is violated by that entity. The critique describes a specific violation, and may provide a solutions to fix it.'
]

{ #category : #accessing }
RenrakuCritiqueHelp class >> bookName [

	^ 'Critiques'
]

{ #category : #accessing }
RenrakuCritiqueHelp class >> pages [

	^ #(RenrakuCritiqueDesignHelp RenrakuCritiqueExamplesHelp)
]
