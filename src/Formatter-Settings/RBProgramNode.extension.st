Extension { #name : 'RBProgramNode' }

{ #category : '*Formatter-Settings' }
RBProgramNode class >> defaultFormatter [

	^ self formatters first

]

{ #category : '*Formatter-Settings' }
RBProgramNode class >> formatters [

	^ (RBAbstractFormatter subclasses reject: [ :each | each isAbstract ])
			sort: [ :a :b | a priority > b priority ]
]

{ #category : '*Formatter-Settings' }
RBProgramNode class >> settingsGroupOn: aBuilder [
	<systemsettings>

	(aBuilder group: #codeFormatters)
		label: 'Code Formatting';
		parent: #codeBrowsing
]

{ #category : '*Formatter-Settings' }
RBProgramNode class >> settingsOn: aBuilder [
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
