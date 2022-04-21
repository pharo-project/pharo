Extension { #name : #ASTProgramNode }

{ #category : #'*Formatter-Settings' }
ASTProgramNode class >> formatters [

	^ ASTProgramNodeVisitor subclasses select: [ :each | each name endsWith: 'Formatter' ]
]

{ #category : #'*Formatter-Settings' }
ASTProgramNode class >> settingsGroupOn: aBuilder [
	<systemsettings>
	
	(aBuilder group: #codeFormatters)
		label: 'Code Formatting';
		parent: #codeBrowsing
]

{ #category : #'*Formatter-Settings' }
ASTProgramNode class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #formatterClass)
		target: ASTProgramNode ;
		domainValues: self formatters; 
		label: 'Default Formatter';
		order: 0;
		default: (Smalltalk at: #EFFormatter);
		description: 'To set the default code formatter.';
		parent: #codeFormatters
]
