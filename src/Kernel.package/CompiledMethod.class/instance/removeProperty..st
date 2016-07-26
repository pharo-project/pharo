accessing-pragmas & properties
removeProperty: propName
	"Remove the property propName if it exists.
	 Do _not_ raise an error if the property is missing."
	| value |
	value := self propertyValueAt: propName ifAbsent: [^nil].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value