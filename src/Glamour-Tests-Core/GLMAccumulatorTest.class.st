Class {
	#name : #GLMAccumulatorTest,
	#superclass : #TestCase,
	#category : 'Glamour-Tests-Core'
}

{ #category : #tests }
GLMAccumulatorTest >> testCopy [
	| accumulator presentation copiedAccumulator |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	(accumulator transmit)
		fromOutsidePort: #selection;
		toOutsidePort: #otherPort.
	copiedAccumulator := accumulator copy.
	self assert: copiedAccumulator transmissions size equals: 1.
	self assert: copiedAccumulator transmissions first ~~ accumulator transmissions first.
	self assert: copiedAccumulator transmissions first destination pane == copiedAccumulator pane.
	self assert: copiedAccumulator transmissions first origins first pane ~~ accumulator pane.
	self assert: copiedAccumulator transmissions first origins first pane == copiedAccumulator pane
]

{ #category : #tests }
GLMAccumulatorTest >> testCustomTransmissions [
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	(accumulator transmit)
		fromOutsidePort: #selection;
		toOutsidePort: #otherPort.
	accumulator startOn: 41.
	(accumulator panes first port: #selection) value: 42.
	self assert: (accumulator pane port: #otherPort) value equals: 42
]

{ #category : #tests }
GLMAccumulatorTest >> testNoDuplicatePaneCreation [
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator entity: 41.
	self assert: accumulator panes size equals: 1.
	accumulator entity: 42.
	self assert: accumulator panes size equals: 2.
	accumulator entity: 41.
	self assert: accumulator panes size equals: 2
]

{ #category : #tests }
GLMAccumulatorTest >> testPaneAddedAnnouncement [
	| accumulator presentation annoumcementsCount |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	annoumcementsCount := 0.
	accumulator when: GLMPaneAdded do: [ :ann | annoumcementsCount := annoumcementsCount + 1 ].
	1 to: 42 do: [ :i | accumulator entity: i ].
	self assert: annoumcementsCount equals: 42
]

{ #category : #tests }
GLMAccumulatorTest >> testPaneCreation [
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator entity: 41.
	self assert: accumulator panes size equals: 1.
	self assert: (accumulator panes first port: #entity) value equals: 41.
	self assert: accumulator panes first presentations first rootPrototype == presentation.
	accumulator entity: 42.
	self assert: accumulator panes size equals: 2.
	self assert: (accumulator panes first port: #entity) value equals: 41.
	self assert: (accumulator panes last port: #entity) value equals: 42.
	self assert: accumulator panes first presentations first rootPrototype equals: presentation.
	self assert: accumulator panes last presentations first rootPrototype equals: presentation
]

{ #category : #tests }
GLMAccumulatorTest >> testPaneName [
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator entity: 41.
	self assert: accumulator panes first name equals: '1'.
	accumulator entity: 42.
	self assert: accumulator panes last name equals: '2'
]

{ #category : #tests }
GLMAccumulatorTest >> testPaneRemovedAnnouncement [
	| accumulator presentation annoumcementsCount |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	annoumcementsCount := 0.
	accumulator when: GLMPaneRemoved do: [ :ann | annoumcementsCount := annoumcementsCount + 1 ].
	1 to: 42 do: [ :i | accumulator entity: i ].
	self assert: annoumcementsCount equals: 0.
	1 to: 42 do: [ :i | accumulator removePaneIndex: 1 ].
	self assert: annoumcementsCount equals: 42
]

{ #category : #tests }
GLMAccumulatorTest >> testPaneSelectedAnnouncement [
	| accumulator presentation annoumcementsCount otherAnnoumcementsCount |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	annoumcementsCount := 0.
	otherAnnoumcementsCount := 0.
	1 to: 42 do: [ :i | accumulator entity: i ].
	accumulator when: GLMPaneSelected do: [ :ann | annoumcementsCount := annoumcementsCount + 1 ].
	accumulator when: GLMPaneAdded do: [ :ann | otherAnnoumcementsCount := otherAnnoumcementsCount + 1 ].
	accumulator when: GLMPaneRemoved do: [ :ann | otherAnnoumcementsCount := otherAnnoumcementsCount + 1 ].
	self assert: annoumcementsCount equals: 0.
	self assert: otherAnnoumcementsCount equals: 0.
	1 to: 42 do: [ :i | accumulator entityToSelect: i ].
	self assert: annoumcementsCount equals: 42.
	self assert: otherAnnoumcementsCount equals: 0
]

{ #category : #tests }
GLMAccumulatorTest >> testPublishToOutside [
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator startOn: 41.
	(accumulator panes first port: #selection) value: 42.
	self assert: (accumulator pane port: #selection) value equals: 42
]

{ #category : #tests }
GLMAccumulatorTest >> testPublishToOutsideWhenEmbedded [
	| browser accumulator actualAccumulator |
	browser := GLMTabulator new.
	accumulator := GLMAccumulator new.
	accumulator addPresentation: GLMPresentation new.
	accumulator
		addTransmission:
			((GLMTransmission new)
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: accumulator;
								name: #selection;
								yourself);
				destination:
						((GLMPresentationBoundPort new)
								presentation: accumulator;
								name: #anotherPort;
								yourself);
				yourself).
	browser addColumn: #one.
	browser
		addTransmission:
			((GLMTransmission new)
				ensurePresentIfNoneStrategy;
				addActiveOrigin:
						((GLMPresentationBoundPort new)
								presentation: browser;
								name: #entity;
								yourself);
				destination: ((browser paneNamed: #one) port: #entity);
				addPresentation: accumulator;
				yourself).
	browser startOn: 41.
	actualAccumulator := browser panes first presentations first.
	(actualAccumulator panes first port: #selection) value: 42.
	self assert: (actualAccumulator pane port: #selection) value equals: 42.
	self assert: (actualAccumulator pane port: #anotherPort) value notNil.
	self assert: (actualAccumulator pane port: #anotherPort) value equals: 42
]

{ #category : #tests }
GLMAccumulatorTest >> testRemovePaneIndex [
	| accumulator presentation |
	accumulator := GLMAccumulator new.
	presentation := GLMPresentation new.
	accumulator addPresentation: presentation.
	accumulator entity: 42.
	accumulator entity: 43.
	accumulator removePaneIndex: 2.
	self assert: accumulator panes size equals: 1
]
