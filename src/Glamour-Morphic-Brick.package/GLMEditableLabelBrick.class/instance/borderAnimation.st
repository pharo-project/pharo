label-brick-accessing
borderAnimation

	^ borderAnimation ifNil: [ borderAnimation := GLMAnimation borderColor duration: 200 ]