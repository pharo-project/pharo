stepping and presenter
arrangeToStartSteppingIn: aWorld
	"Start getting sent the 'step' message in aWorld.  Like startSteppingIn:, but without the initial one to get started'"

	aWorld ifNil: [ ^ self ].
	aWorld startStepping: self.
	self changed.