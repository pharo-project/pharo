Extension { #name : #DebugAction }

{ #category : #'*GT-Debugger' }
DebugAction >> asGlamourAction [

	^ ((GLMGenericAction new) 
			action: [ self execute ]; 
			icon: self icon;
			title: self label;
			help: self help;
			showTitle;
			shortcut: (self keyText ifNotNil: [:aString | aString at: 1]);
			category: self category;
			yourself)
]

{ #category : #'*GT-Debugger' }
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

{ #category : #'*GT-Debugger' }
DebugAction >> specId [

	^ (self id, 'SpecId') asSymbol
]

{ #category : #'*GT-Debugger' }
DebugAction >> specModel [

	^ #GTDebugActionButton
]
