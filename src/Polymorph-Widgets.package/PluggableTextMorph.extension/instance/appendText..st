*Polymorph-Widgets
appendText: aTextOrString
	"Append the given text to the receiver."

	self handleEdit: [
		self 
			selectInvisiblyFrom: textMorph asText size + 1 to: textMorph asText size;
			replaceSelectionWith: aTextOrString;
			selectFrom: textMorph asText size + 1 to: textMorph asText size;
			hasUnacceptedEdits: false;
			scrollSelectionIntoView;
			changed]