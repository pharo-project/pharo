menus behavior
copyClasses
	
	| association |
	self okToChange ifFalse: [^ self].
	association := self copyClasses: (self selectedClasses collect: [:e | e theNonMetaClass ]).
	association key
		ifTrue: [
			self selectedClass: association value.
			self updateClassView]