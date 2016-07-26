transforming
transform
	| replacement refactoring |
	self model allClassesDo: [ :class |
		(class isNil or: [ class isMeta ]) ifFalse: [
			replacement := self execute: class name.
			replacement = class name asString ifFalse: [
				refactoring := self perform: mode
					with: class with: replacement asSymbol.
				(refactoring notNil and: [ refactoring preconditions check ])
					ifTrue: [ refactoring transform ] ] ] ]