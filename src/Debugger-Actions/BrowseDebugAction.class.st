"
A BrowseDebugAction is a debugging actions sending a unary message to the stack widget.

"
Class {
	#name : #BrowseDebugAction,
	#superclass : #MessageSendDebugAction,
	#category : #'Debugger-Actions-Actions'
}

{ #category : #'actions creation' }
BrowseDebugAction class >> debugActionsFor: aDebugger [
	<contextMenuDebuggingAction>
	| initilOrder |
	
	initilOrder := 60.
	^ { 
		self new
			id: #browseSendersOfMessages;
			order: initilOrder;
			keymap: PharoShortcuts current browseSendersShortcut;
			help: 'Browse the senders of the current message.';
			label: 'Senders of...'.
		self new
			id: #browseMessages;
			order: initilOrder + 5;
			keymap: PharoShortcuts current browseImplementorsShortcut;
			help: 'Browse the implementors of the current message.';
			label: 'Implementors of...'.
		self new
			id: #methodHierarchy;
			order: initilOrder + 10;
			keymap: PharoShortcuts new browseMethodHierarchyShortcut;
			help: 'Browser a hierarchy of the implementors of the current mnethod.';
			label: 'Inheritance'.
		self new
			id: #browseVersions;
			order: initilOrder + 15;
			keymap: PharoShortcuts current browseVersionsShortcut;
			help: 'Open the historic of the message.';
			label: 'Versions'.
		self new
			id: #browseInstVarRefs;
			order: initilOrder + 20;
			help: 'Browse the references to the instance variable.';
			label: 'Inst var refs...'.
		self new
			id: #browseClassVarRefs;
			order: initilOrder + 25;
			help: 'Browse the references to the class variable.';
			label: 'Class var refs...'.
		self new
			id: #browseClassVariables;
			order: initilOrder + 30;
			help: 'Browse the class variables.';
			label: 'Class variables';
			withSeparatorAfter.
			
		self new
			id: #browseClassRefs;
			order: initilOrder + 35;
			keymap: PharoShortcuts current browseClassReferencesShortcut;
			help: 'Browse the references to the class.';
			label: 'Class refs'.
		self new
			id: #browseMethodFull;
			order: initilOrder + 40;
			keymap: PharoShortcuts current browseShortcut;
			help: 'Open a system browser on the method sent.';
			label: 'Browse full'.
		self new
			id: #browseReceiver;
			order: initilOrder + 41;
			help: 'Open a system browser on the class of the object receiving the method.';
			"keymap: PharoShortcuts current browseShortcut;"
			label: 'Browse receiver'.
		
		self new
			id: #fileOutMessage;
			order: initilOrder + 45;
			help: 'Export the code of the current method to a file.';
			label: 'File out'.
		self new
			id: #inspectInstances;
			order: initilOrder + 50;
			help: 'Open an inspector on a collection containing all the instances of the receiver class.';
			label: 'Inspect instances'.
		self new
			id: #inspectSubInstances;
			order: initilOrder + 55;
			help: 'Open an inspector on a collection containing all the instances and sub instances of the receiver class.';
			label: 'Inspect sub instances';
			withSeparatorAfter
	 }
]

{ #category : #initialization }
BrowseDebugAction >> initialize [

	super initialize.
	self needsUpdate: false.
	self needsValidation: false.
]

{ #category : #accessing }
BrowseDebugAction >> receiver [

	^ self debugger stack
]
