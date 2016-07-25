theme
themeChanged
	self isSelected
		ifTrue: [ self themer tabLabelThemer selectedStyleFor: self ]
		ifFalse: [ self themer tabLabelThemer normalStyleFor: self ].
	super themeChanged