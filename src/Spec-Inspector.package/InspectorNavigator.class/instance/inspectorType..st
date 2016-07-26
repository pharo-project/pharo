accessing
inspectorType: anInspectorClass
	"Use a new inspector type. Instantiate the given inspector class and replace the current view"
	| oldScriptAreaTextModel |
	
	oldScriptAreaTextModel := self inspector text.
	
	anInspectorClass = self inspector class
		ifTrue: [  ^ self ].
	anInspectorClass isNil
		ifTrue: [ ^ self ].
		
	(self inspectorTypes includes: anInspectorClass)
		ifFalse: [ ^ self ].
		
	self basicInspector: (anInspectorClass new 
		inspect: self object;
		yourself).

	self inspector text 
		text: oldScriptAreaTextModel getText;
		setSelection: oldScriptAreaTextModel getSelection.

	history replaceCurrentWith: self inspector.
	self toolbar update.