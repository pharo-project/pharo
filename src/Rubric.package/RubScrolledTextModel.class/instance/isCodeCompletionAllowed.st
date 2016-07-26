necompletion-extensions
isCodeCompletionAllowed
	| allowed |
	self reconfigureViewWith: [ :scrolledText | allowed := scrolledText textArea editingMode isCodeCompletionAllowed ].
	^ allowed