"
see superClass 's documentation
my action is to browse the selectedItem in CriticsPresenter 

"
Class {
	#name : #CBBrowseCriticAction,
	#superclass : #CBAbstractCriticAction,
	#category : #'Tool-CriticBrowser-utilities-buttons'
}

{ #category : #accessing }
CBBrowseCriticAction class >> groupPriority [
	^ 1
]

{ #category : #accessing }
CBBrowseCriticAction class >> runOn: aCriticBrowser [
	aCriticBrowser selectedCritic
		ifNotNil: [ :element | 
			| entity |
			entity := element sourceAnchor entity.
			entity isBehavior
				ifTrue: [ entity instanceSide browse ]
				ifFalse: [ entity browse ] ]
		ifNil: [ self inform: 'No critic selected' ]
]

{ #category : #accessing }
CBBrowseCriticAction class >> title [
	^ 'Browse'
]
