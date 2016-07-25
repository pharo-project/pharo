accessing
contents: aString withMarkers: aBool inverse: inverse 
	"Set the menu item entry. If aBool is true, parse aString for embedded markers."

	| markerIndex marker |
	self contentString: nil.	"get rid of old"
	aBool ifFalse: [^super contents: aString].
	self removeAllMorphs.	"get rid of old markers if updating"
	self hasIcon ifTrue: [ self icon: nil ].
	self flag: #CleanYesNoOnOffAfter.
	(aString isKindOf: Association)
		ifTrue: [ super contents: aString value. 
				marker := aString key   
					ifTrue: [self onImage]
					ifFalse: [self offImage]]
		ifFalse: [
		(aString notEmpty and: [aString first = $<]) 
			ifFalse: [^super contents: aString].
		markerIndex := aString indexOf: $>.
		markerIndex = 0 ifTrue: [^super contents: aString].
	marker := (aString copyFrom: 1 to: markerIndex) asLowercase.
	(#('<on>' '<off>' '<yes>' '<no>') includes: marker) 
		ifFalse: [^super contents: aString].
	self contentString: aString.	"remember actual string"
	marker := (marker = '<on>' or: [marker = '<yes>']) ~= inverse 
				ifTrue: [self onImage]
				ifFalse: [self offImage].
	super contents:  (aString copyFrom: markerIndex + 1 to: aString size)].
	"And set the marker"
	marker := ImageMorph new form: marker.
	marker position: self left @ (self top + 2).
	self addMorphFront: marker