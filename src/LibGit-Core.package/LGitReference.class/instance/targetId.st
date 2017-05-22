accessing
targetId
	| data oId |
	data := self reference_target: self.
	data getHandle isNull
		ifTrue: [ ^ nil ].
	oId := LGitId fromExternalAddress: data.
	oId internalize.
	^ oId