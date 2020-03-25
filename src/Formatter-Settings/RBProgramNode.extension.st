Extension { #name : #RBProgramNode }

{ #category : #'*Formatter-Settings' }
RBProgramNode class >> formatters [

	^ RBProgramNodeVisitor subclasses select: [ :each | each name endsWith: 'Formatter' ]
]

{ #category : #'*Formatter-Settings' }
RBProgramNode class >> settingsGroupOn: aBuilder [
	<systemsettings>
	
	(aBuilder group: #codeFormatters)
		label: 'Code Formatting';
		parent: #codeBrowsing
]

{ #category : #'*Formatter-Settings' }
RBProgramNode class >> settingsOn: aBuilder [
	<systemsettings>
	(aBuilder pickOne: #formatterClass)
		target: RBProgramNode ;
		domainValues: self formatters; 
		label: 'Default Formatter';
		order: 0;
		default: (Smalltalk at: #BIConfigurableFormatter);
		description: 'To set the default code formatter.';
		parent: #codeFormatters
]
