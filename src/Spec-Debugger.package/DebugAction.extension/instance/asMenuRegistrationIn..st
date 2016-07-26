*spec-debugger
asMenuRegistrationIn: aBuilder
	| item |
	
	item := (aBuilder item: self specId)
		label: self label, (self keyText ifNil: [ '' ] ifNotNil: [ '  (', self keyText, ')' ] );
		icon: self icon;
		enabled: self enabled;
		order: self order asFloat;
		target: self; 
		selector: #execute;
		arguments: #().
	self needsSeparatorAfter 
		ifTrue: [ item withSeparatorAfter ].
	^ item
