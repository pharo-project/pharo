accessing-pragmas & properties
removeProperty: propName ifAbsent: aBlock
	"Remove the property propName if it exists.
	 Answer the evaluation of aBlock if the property is missing."
	| value |
	value := self propertyValueAt: propName ifAbsent: [^aBlock value].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value