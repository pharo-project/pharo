private
getFirstBuffer
	"Get the first source buffer after initialization has been done"
	sourceStream == nil ifTrue:[^self].
	source := sourceStream next: 1 << 16. "This is more than enough..."
	sourceLimit := source size.