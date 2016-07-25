mock for spec
enabled: aBoolean
	enabled = aBoolean ifTrue: [^self].
	enabled := aBoolean.
	self textArea readOnly: aBoolean not.
	self changed