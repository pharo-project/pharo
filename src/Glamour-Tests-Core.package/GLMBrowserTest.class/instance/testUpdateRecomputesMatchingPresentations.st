tests
testUpdateRecomputesMatchingPresentations
	| browser collection matchingPresentationsChangedAnnounced | 
	collection := GLMAnnouncingCollection new.
	collection add: 1.
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [:a | 
		a list when: [:col | col isEmpty not ]].
	browser startOn: collection.
	matchingPresentationsChangedAnnounced := false.
	browser panes first when: GLMMatchingPresentationsChanged do: [:ann | 
		matchingPresentationsChangedAnnounced := true ].
	collection removeLast.
	browser update.
	self assert: matchingPresentationsChangedAnnounced