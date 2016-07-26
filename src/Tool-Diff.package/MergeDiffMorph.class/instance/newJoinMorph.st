actions
newJoinMorph
	"Answer a new join morph."

	^super newJoinMorph
		when: #joinClicked send: #update: to: self with: #joinClicked