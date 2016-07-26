drawing
fullDrawOnAthensCanvas: anAthensCanvas 
	"Draw the full Morphic structure on the given Canvas"

	self visible ifFalse: [^ self].
	(anAthensCanvas isVisible: self fullBounds) ifFalse: [^self].
	(self hasProperty: #errorOnDraw) ifTrue: [^self "drawErrorOn: aCanvas" ].

	(anAthensCanvas isVisible: self bounds) ifTrue: [ anAthensCanvas draw: self ].
	
	submorphs isEmpty ifTrue: [^self].

	self clipSubmorphs 
		ifTrue: [
			anAthensCanvas 
				clipBy: self clippingBounds
				during: [ submorphs reverseDo: [ :m | anAthensCanvas fullDrawMorph: m ] ] ] 
		ifFalse: [ submorphs reverseDo: [ :m | anAthensCanvas fullDrawMorph: m ] ]
