event handling - private
inform: evt to: aLeftMorph originatedFrom: anEvent ifNotFocusedDo: aBlock

	^ (self is: anEvent withFocusOver: aLeftMorph) 
		ifTrue: [ self transform: evt from: anEvent andSendTo: aLeftMorph ]
		ifFalse: aBlock