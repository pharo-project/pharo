*Ring-Core-Kernel
methodReference

	| class selector |
	class := self methodClass ifNil: [^nil].
	selector := self selector ifNil: [^nil].
	^RGMethodDefinition realClass: class selector: selector.
	