"
I implement a really simple memory game using the dynamic presenters lists builder of Spec.

The goal of the game is to find all pairs of cards having the same number.

The purpose is to show that with DynamicPresentersListBuilder, you can easily build dynamic presenters containing a list of presenters for which the size depends of some objects provided as input.

The slider triggers rebuilds of the dynamic presenter in order to update the number of cards in the game.
"
Class {
	#name : #SpDemoDynamicMemoryGamePresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'dynamicPresenter',
		'builder',
		'differentCardsCountSlider'
	],
	#category : #'Spec2-Examples-Demo-Dynamic'
}

{ #category : #specs }
SpDemoDynamicMemoryGamePresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #dynamicPresenter;
		add: #differentCardsCountSlider withConstraints: [ :constraints | constraints height: self buttonHeight ];
		yourself
]

{ #category : #example }
SpDemoDynamicMemoryGamePresenter class >> example [
	^ self new openWithSpec
]

{ #category : #accessing }
SpDemoDynamicMemoryGamePresenter >> cardPairsCount [
	^ self differentCardsCountSlider value
]

{ #category : #accessing }
SpDemoDynamicMemoryGamePresenter >> differentCardsCountSlider [
	^ differentCardsCountSlider
]

{ #category : #accessing }
SpDemoDynamicMemoryGamePresenter >> dynamicPresenter [
	^ dynamicPresenter
]

{ #category : #accessing }
SpDemoDynamicMemoryGamePresenter >> dynamicPresenter: anObject [
	dynamicPresenter := anObject
]

{ #category : #'game logic' }
SpDemoDynamicMemoryGamePresenter >> foundIcon [
	^ self iconNamed: #glamorousAccept
]

{ #category : #'game logic' }
SpDemoDynamicMemoryGamePresenter >> gameLogicWithShowedCards: showedCards objectConcerned: modelObject andAssociatedPresenter: presenter [
	"The game logic, not as clean as it could be but the goal of this example is not to show how to design a memory game."
	presenter
		icon: self hiddenIcon;
		action: [ (presenter icon = self hiddenIcon and: [ showedCards size < 2 ])
				ifTrue: [ showedCards add: presenter.
					presenter
						icon: nil;
						label: modelObject asString.
					showedCards size = 2
						ifTrue: [ [ (1 / 2) second wait.
							showedCards first label asString
								= showedCards second label asString
								ifTrue: [ showedCards
										do: [ :p | 
											p
												icon: self foundIcon;
												label: nil ] ]
								ifFalse: [ showedCards
										do: [ :p | 
											p
												icon: self hiddenIcon;
												label: nil ] ].
							showedCards removeAll ] fork ] ] ]
]

{ #category : #'game logic' }
SpDemoDynamicMemoryGamePresenter >> hiddenIcon [
	^ self iconNamed: #comments
]

{ #category : #initialization }
SpDemoDynamicMemoryGamePresenter >> initializePresenters [
	dynamicPresenter := self newNullPresenter.

	differentCardsCountSlider := self newSlider.
	
	differentCardsCountSlider
		min: 1;
		max: 50;
		label: '# cards';
		whenValueChangedDo: [ self rebuildDynamicPresenter ];
		value: 25
]

{ #category : #'dynamic presenter' }
SpDemoDynamicMemoryGamePresenter >> rebuildDynamicPresenter [
	| allCards showedCards |
	"Each card is duplicated and the resulting set is shuffled."
	allCards := ((1 to: self cardPairsCount) , (1 to: self cardPairsCount)) shuffled.
	"This var. holds the set of cards actually shown to the user."
	showedCards := OrderedCollection new.
	"Build the presenters list depending on the total number of cards."
	builder := SpDynamicPresentersListBuilder new
		modelObjects: allCards;
		presenter: SpButtonPresenter "We use buttons as UI for cards."
			configuredAs: [ :presenter :modelObject | 
			self
				gameLogicWithShowedCards: showedCards
				objectConcerned: modelObject
				andAssociatedPresenter: presenter ];
		"We specify that the cards should be organized on 10 columns, placed from left to right."
		layoutBuilder: (SpDynamicLeftToRightColumnsLayout columns: 10);
		yourself.
		
	"Update the dynamic presenter using the builder (the symbol provided as parameter is the accessor.
	 The mutator must be this accessor with $: as suffix (i.e., #dynamicPresenter:)."
	self newDynamicPresentersListIn: #dynamicPresenter usingBuilder: builder
]
