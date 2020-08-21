"
I define settings to configure debugger extensions, *i.e.*, all users of `TStDebuggerExtension`.
"
Class {
	#name : #StDebuggerExtensionSettings,
	#superclass : #Object,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #settings }
StDebuggerExtensionSettings class >> debuggerExtensionSettingsForClass: toolClass builder: aBuilder [

	| toolName |
	toolName := toolClass basicNew debuggerExtensionToolName.

	(aBuilder group: toolName asSymbol)
		label: toolName;
		with: [ 
			(aBuilder setting: #showInDebugger)
				label: 'Show in debugger';
				target: toolClass;
				default: toolClass showInDebugger;
				description: 'Activate the tool tab in the debugger right column.'.
			(aBuilder pickOne: #displayOrder)
				label: 'Position';
				target: toolClass;
				default: toolClass defaultDisplayOrder;
				domainValues: (1 to: 100) asArray;
				description:
					'Position of the tool tab in the debugger right column.' ]
]

{ #category : #settings }
StDebuggerExtensionSettings class >> debuggerExtensionSettingsOn: aBuilder [

	<systemsettings>
	| debuggerClass |
	debuggerClass := StDebugger.
	(aBuilder group: #debuggerExtensions)
		label: 'Debugger Extensions';
		parent: #debugging;
		description:
			'Tools that extend the StDebugger and appear in the right column of the debugger';
		with: [ 
			| settingsGroup |
			settingsGroup := aBuilder setting: #activateDebuggerExtensions.
			settingsGroup label: 'Activate extensions'.
			settingsGroup target: debuggerClass.
			settingsGroup default: true.
			settingsGroup description:
					'(De)Activate the debugger extensions tools in the debugger right column.'.
			settingsGroup with: [ 
					debuggerClass extensionToolsClasses do: [ :toolClass | 
							self
								debuggerExtensionSettingsForClass: toolClass
								builder: aBuilder ] ] ]
]
