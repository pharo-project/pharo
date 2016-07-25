updating
update: aParameter 
	"Refer to the comment in View|update:."

	aParameter == self getStateSelector ifTrue: 
		[self updateSelection.
		^ self].
	aParameter == self getEnabledSelector ifTrue:
		[self updateEnabled.
		^ self].
	aParameter == self getLabelSelector ifTrue:
		[self updateLabel.
		^ self].
	
	aParameter isArray ifFalse: [ ^ self ].
	aParameter size == 2 ifFalse: [ ^ self ].
	
	aParameter first = #labelClickable: ifTrue: [ self labelClickable: aParameter second ].
	aParameter first = #label: ifTrue: [ self label: aParameter second ]