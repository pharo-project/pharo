private
setAttributesIn: aText fromRanges: ranges 
	"use the . to find and assign TextAttributes to aText"
	
	| charAttr oldRuns |
	
	oldRuns := aText runs.
	charAttr := Array new: aText size withAll: (self attributesFor: #default).
	
	ranges do: [:range | 
		(self attributesFor: range type) ifNotNil: [ :attribute|
			charAttr 
				from: range start
				to: range end
				put: attribute]].

	self addLinkAttributesFor: ranges in: aText to: charAttr.
	
	aText runs: (RunArray newFrom: charAttr).
	
	self 
		addAttributesFrom: oldRuns 
		satisfying: [:each | each shoutShouldPreserve] 
		to: aText.