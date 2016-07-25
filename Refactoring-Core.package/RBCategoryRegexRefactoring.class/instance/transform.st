transforming
transform
	| replacement |
	self model allClassesDo: [ :class |
		(class isNil or: [ class isMeta ]) ifFalse: [
			replacement := self execute: class category.
			replacement = class category asString ifFalse: [
				class category: replacement.
				self model defineClass: class definitionString ] ] ]