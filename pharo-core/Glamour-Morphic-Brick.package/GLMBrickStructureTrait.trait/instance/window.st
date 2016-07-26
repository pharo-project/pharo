brick-structure-opening
window
	"Answer the receiver's window."
	#GLMWindowBrick asClassIfAbsent: [ 
		^ self ownerThatIsA: SystemWindow ].
	
	^ (self ownerThatIsA: #GLMWindowBrick asClass)
		ifNil: [ self ownerThatIsA: SystemWindow ]