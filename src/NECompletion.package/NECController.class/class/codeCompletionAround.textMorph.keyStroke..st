accessing
codeCompletionAround: aBlock textMorph: aTextMorph keyStroke: evt
	"Inserts code completion if allowed in this morph"
	
	| editor stringHolder completionAllowed controller |
	
	editor := aTextMorph editor.
	stringHolder := editor ifNotNil:[ editor model ].
	completionAllowed := self allowModel: stringHolder.
	completionAllowed 
		ifTrue: [ 
			controller := self uniqueInstance.
			controller setModel: stringHolder.
			(controller handleKeystrokeBefore: evt editor: editor)
				ifTrue:  [^ self ] ].

	aBlock value.
	
	completionAllowed
		ifTrue: [ controller handleKeystrokeAfter: evt editor: editor ]