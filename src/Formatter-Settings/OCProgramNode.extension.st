Extension { #name : 'OCProgramNode' }

{ #category : '*Formatter-Settings' }
OCProgramNode class >> defaultFormatter [

	^ self formatters first

]

{ #category : '*Formatter-Settings' }
OCProgramNode class >> formatters [

	^ (OCAbstractFormatter subclasses reject: [ :each | each isAbstract ])
			sort: [ :a :b | a priority > b priority ]
]

{ #category : '*Formatter-Settings' }
OCProgramNode class >> settingsGroupOn: aBuilder [
	<systemsettings>

	(aBuilder group: #codeFormatters)
		label: 'Code Formatting';
		parent: #codeBrowsing
]

{ #category : '*Formatter-Settings' }
OCProgramNode class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #formatterClass)
		target: self ;
		domainValues: self formatters;
		label: 'Default Formatter';
		order: 0;
		default: self defaultFormatter;
		description: 'To set the default code formatter.';
		parent: #codeFormatters
]
