accessing rulers
withGhostText: aText
	self withRulerNamed: #ghostText.
	self ghostTextRuler updateTextWith: aText asText.
	self ghostTextRuler comeToFront