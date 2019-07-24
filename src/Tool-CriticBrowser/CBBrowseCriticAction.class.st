"
see superClass 's documentation
my action is to browse the selectedItem in CriticsPresenter 

"
Class {
	#name : #CBBrowseCriticAction,
	#superclass : #CBAbstractCriticAction,
	#category : #'Tool-CriticBrowser-Actions'
}

{ #category : #accessing }
CBBrowseCriticAction class >> groupPriority [
	^ 1
]

{ #category : #testing }
CBBrowseCriticAction class >> runOn: aCriticBrowser [
	aCriticBrowser selectedCritic ifNotNil: [ :element | element entity browse ] ifNil: [ self inform: 'No critic selected' ]
]

{ #category : #accessing }
CBBrowseCriticAction class >> title [
	^ 'Browse'
]
