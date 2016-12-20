brick-structure-opening
window
	"Answer the receiver's window."
	
	^ self class environment at: #GLMWindowBrick 
		ifPresent: [ :glmWindowBrick |
			(self ownerThatIsA: glmWindowBrick)
				ifNil: [ self ownerThatIsA: SystemWindow ]]
		ifAbsent: [ self ownerThatIsA: SystemWindow ].