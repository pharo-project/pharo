updating
update: aParameter 
	|state|
	
	aParameter ifNil: [^self].
	"I associate label and icon because both are part of a 'complex' label... and update one 
	 means update the other"
	((aParameter == getLabelSelector) or: [ (aParameter == getIconSelector) ]) ifTrue: [
		getIconSelector ifNotNil: [ self icon: (model perform: getIconSelector) ].
		self label: (getLabelSelector ifNotNil: [ model perform: getLabelSelector])].
	aParameter == getFontSelector ifTrue: [ self font: (model perform: getFontSelector) ].
	state := self getModelState.
	(state ~= (self lastState) 
		or: [ getStateSelector isNil and: [aParameter == #onOffColor]])
		ifTrue: [
			self color: self colorToUse.
			self lastState: state].
	aParameter == getEnabledSelector ifTrue: [^self enabled: (model perform: getEnabledSelector)].
	getColorSelector ifNotNil: [ | cc |
		color = (cc := model perform: getColorSelector) 
			ifFalse: [
				self privateColor: cc.
				self onColor: color offColor: color.
				self changed ]].
		
	aParameter isArray ifFalse: [ ^ self ].
	aParameter size == 2 ifFalse: [ ^ self ].
	
	aParameter first = #askBeforeChanging: ifTrue: [ self askBeforeChanging: aParameter second ]