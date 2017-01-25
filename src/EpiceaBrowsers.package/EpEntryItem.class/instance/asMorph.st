converting
asMorph
	| morphs |
	morphs := OrderedCollection new.

	morphs addAll: self triggerMarkWidgets.

	morphs add: self icon asMorph.

	"create the main display"	
	morphs addAll: (entry content accept: EpMorphVisitor new).

	"add a small gap"
	morphs add: EpMorphFactory default smallGap.

	"add time"
	self timeString ifNotEmpty: [ :timeString |
		morphs add: (EpMorphFactory default secondaryStringFor: timeString) ].

	"maybe add comment tag"
	browser commentsLog ifNotNil: [ :commentsLog | commentsLog
		commentAt: entry 
		ifPresent: [ :comment |
			comment trim isEmpty ifFalse: [
				morphs add: (EpMorphFactory default pillWith: comment trim) ] ] ].

	"create the row morph"	
	^ EpMorphFactory default rowWithAll: morphs