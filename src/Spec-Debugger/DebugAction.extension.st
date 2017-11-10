Extension { #name : #DebugAction }

{ #category : #'*spec-debugger' }
DebugAction >> asMenuRegistrationIn: aBuilder [
	| item |
	item := (aBuilder item: self specId)
		label: self label, (self keyText ifNil: [ '' ] ifNotNil: [ '  (', self keyText, ')' ] );
		setBalloonText: self help;
		icon: self icon;
		enabled: self enabled;
		order: self order asFloat;
		target: self; 
		selector: #execute;
		arguments: #().
	self needsSeparatorAfter 
		ifTrue: [ item withSeparatorAfter ].
	^ item

]

{ #category : #'*spec-debugger' }
DebugAction >> specId [

	^ (self id, 'SpecId') asSymbol
]

{ #category : #'*spec-debugger' }
DebugAction >> specModel [

	^ #SpecDebugActionButton
]
