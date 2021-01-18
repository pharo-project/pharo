Extension { #name : #StDebugger }

{ #category : #'*NewTools-Sindarin-Commands' }
StDebugger class >> buildSindarinExtentionCommandsGroupWith: stDebuggerInstance forRoot: rootCommandGroup [

	<extensionCommands>
	| commands codeGroup toolbarGroup toolbarSindarinGroup |
	commands := (Pragma allNamed: #codeExtensionDebugCommand:) collect: [ 
		            :prg | 
		            | class |
		            class := prg methodClass soleInstance.
		            class forSpecContext: stDebuggerInstance ].


	codeGroup := rootCommandGroup
	             / StDebuggerCodeCommandTreeBuilder groupName
	             / StDebuggerCodeCommandTreeBuilder new codeDebugCommandsGroupName.
	commands do: [ :c | codeGroup register: c ].

	toolbarGroup := rootCommandGroup / StDebuggerToolbarCommandTreeBuilder groupName.
	toolbarSindarinGroup := CmCommandGroup forSpec
			beToolbarPopoverButton;
			name: 'Advanced Step';
			icon: (stDebuggerInstance application iconNamed: #smallExpert);
			yourself.
	toolbarGroup register: toolbarSindarinGroup.
	commands do: [ :c | toolbarSindarinGroup register: c ].		 
]
