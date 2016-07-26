instance-creation
newCodeMorph
	"Answer a new code morph."

	^(self newTextEditorFor: self
		getText: #compositeText
		setText: #compositeText:
		getEnabled: nil)
		wrapFlag: false;
		setText: ''