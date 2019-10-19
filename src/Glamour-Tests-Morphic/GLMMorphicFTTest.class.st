Class {
	#name : #GLMMorphicFTTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #asserting }
GLMMorphicFTTest >> assertNoDuplicatedAnnouncementsIn: anAnnouncement [
	| duplications |
	duplications := (anAnnouncement subscriptions subscriptions
		groupedBy: [ :aSubscription | aSubscription announcementClass ])
		select: [ :each | each value size > 1 ].
	self
		assert: duplications isEmpty
		description: 'The announcement should have only one subscription of a given type.'
]

{ #category : #tests }
GLMMorphicFTTest >> testNoDuplicateRegistrationOfAnnouncementsOnDataSource [
	| table amountOfMenuItemSelectedSubscriptions |
	window := GLMCompositePresentation new
		with:
				[ :c | c fastList selectionAct: [ 42 inspect ] entitled: 'Answer' ];
		openOn: (1 to: 42).
	table := self find: FTTableMorph in: window.
	amountOfMenuItemSelectedSubscriptions := table dataSource announcer
		subscriptions subscriptions
		count: [ :each | each announcementClass = GLMMenuItemSelected ].
	self assert: amountOfMenuItemSelectedSubscriptions equals: 1
]

{ #category : #tests }
GLMMorphicFTTest >> testNoDuplicateRegistrationsInFastTableRenderer [
	| table |
	window := GLMCompositePresentation new
	  with: [ :c |
	    c fastList ];
	   openOn: (1 to: 42).

	table := self find: FTTableMorph in: window.
	self assertNoDuplicatedAnnouncementsIn: table announcer.
	self assertNoDuplicatedAnnouncementsIn: table dataSource announcer.
]

{ #category : #tests }
GLMMorphicFTTest >> testTypeOfMultipleSelection [
	| composer firstTreeMorph treeNodeMorph |
	composer := GLMCompositePresentation new.
	composer fastList beMultiple.
	window := composer
		openOn:
			{{1} asSet.
			{1} asOrderedCollection}.
	firstTreeMorph := self find: FTTableMorph in: window.
	treeNodeMorph := firstTreeMorph selectionStrategy toggleIndex: 1.
	self
		assert: composer selection asOrderedCollection
		equals: {{1} asSet} asOrderedCollection
]
