"
see superClass 's documentation
my action is :
	to tag the selected critic in the criticsPresenter 
"
Class {
	#name : #CBFalsePositiveCriticAction,
	#superclass : #CBAbstractCriticAction,
	#category : #'Tool-CriticBrowser-utilities-buttons'
}

{ #category : #accessing }
CBFalsePositiveCriticAction class >> groupPriority [
	^ 20
]

{ #category : #accessing }
CBFalsePositiveCriticAction class >> runOn: aCriticBrowser [
	aCriticBrowser selectedCritic
		ifNotNil: [ :item | 
			aCriticBrowser markAsFalsePositiveForCurrentRule: item ]
		ifNil: [ self inform: 'No critic selected' ]
]

{ #category : #accessing }
CBFalsePositiveCriticAction class >> title [
	^ 'Mark as wrong'
]
