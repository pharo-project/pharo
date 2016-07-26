private
parseMethod: aString
	^ [ RBParser parseMethod: aString ]
		on: Error do: [ :err | nil ]