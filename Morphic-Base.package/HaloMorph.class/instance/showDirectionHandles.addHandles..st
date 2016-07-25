private
showDirectionHandles: wantToShow addHandles: needHandles 
	directionArrowAnchor := wantToShow 
				ifTrue: [target referencePositionInWorld	"not nil means show"]
				ifFalse: [nil].
	needHandles ifTrue: [self addHandles] 