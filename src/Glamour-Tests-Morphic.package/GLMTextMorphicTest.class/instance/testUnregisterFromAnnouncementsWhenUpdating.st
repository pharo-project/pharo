tests
testUnregisterFromAnnouncementsWhenUpdating
	| composite announcingCollection numberOfExecutions |
	numberOfExecutions := 0.
	announcingCollection := GLMAnnouncingCollection new.
	composite :=  GLMCompositePresentation new with: [ :a | 
					(a text)
						updateOn: GLMItemAdded from: [ announcingCollection ];
						display: [ numberOfExecutions := numberOfExecutions + 1 ] ].
	window := composite openOn: announcingCollection.
	1 to: 3 do: [ :each | announcingCollection add: each ].
	self assert: numberOfExecutions equals: 4	"it should not be 8"