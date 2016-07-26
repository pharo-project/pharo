tests
testPaneSelectedAnnouncement
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