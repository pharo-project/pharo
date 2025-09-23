Extension { #name : 'String' }

{ #category : '*Deprecated14' }
String >> trimmed [
	"Trim separators from both sides of the receiving string."

	"'  hello  ' trimmed >>> 'hello'"

	"'hello' trimmed >>> 'hello'"

	"'' trimmed >>> ''"

	self
		backwardCompatible: 'Use trimBoth instead -> https://github.com/pharo-project/pharo/issues/18307'
		on: '28 June 2025'
		in: 'pharo14'
		transformWith: '`@receiver trimmed' -> '`@receiver trimBoth'.


	^ self trimBoth
]
