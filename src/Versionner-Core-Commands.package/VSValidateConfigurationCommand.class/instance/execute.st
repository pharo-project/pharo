execute
execute

	| result |
	result := self validate.
	result ifEmpty: [ UIManager default inform: 'The configuration is correctly defined'. ^ self ].

	UIManager default inform: 'Issues were found'.
	result := result collect: [ :each | each explanation ].
	result inspect