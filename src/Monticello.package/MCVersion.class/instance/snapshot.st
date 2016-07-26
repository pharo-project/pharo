accessing
snapshot
	"lazily load snapshot..."
	snapshot isBlock ifTrue: [ 
		snapshot := snapshot value ].
	^ snapshot