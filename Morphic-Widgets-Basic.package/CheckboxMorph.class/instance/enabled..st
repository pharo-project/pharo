accessing
enabled: aBoolean
	"Set the value of enabled"

	enabled := aBoolean.
	self labelMorph ifNotNil: [:m | m enabled: aBoolean].
	self buttonMorph ifNotNil: [:m | m enabled: aBoolean].
	self changed: #enabled