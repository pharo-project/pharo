populating browser
displayWidget
	| morphs |
	morphs := OrderedCollection new.

	morphs addAll: self triggerMarkWidgets.

	morphs add: self icon asMorph.

	"create the main display"	
	morphs addAll: (entry content accept: EpDisplayMorphVisitor new).

	"add a small gap"
	morphs add: self smallGapWidget.

	"add author"
	self authorString ifNotEmpty: [ :authorString |
		morphs add: (self 
			smallStringWidgetFor: authorString
			color: Color transparent) ].

	"add time"
	self timeString ifNotEmpty: [ :timeString |
		morphs add: (self 
			smallStringWidgetFor: timeString
			color: Color transparent) ].

	"maybe add comment tag"
	browser commentsLog ifNotNil: [ :commentsLog | commentsLog
		commentAt: entry 
		ifPresent: [ :comment |
			comment trim isEmpty ifFalse: [
				morphs add: (
					self 
						tagWidgetFor: comment 
						color: Color lightYellow) ] ] ].
	"maybe add commit tag"
	browser commitLog ifNotNil: [ browser commitLog
		commitAt: entry 
		ifPresent: [ :commit |
			commit trim isEmpty ifFalse: [
				morphs add: (
					self 
						tagWidgetFor: commit 
						color: Color lightGreen) ] ] ].

	"maybe add head tag"
"	browser offLog headReference isNull ifFalse: [
		entry = self log head 
			ifTrue: [ 
				morphs add: (
					self 
						tagWidgetFor: 'head'
						color: Color blue muchLighter) ] ]."

	"create the row morph"	
	^ self panelWidgetWithAll: morphs