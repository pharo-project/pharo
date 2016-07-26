system integration
systemMethodModifiedActionFrom: ann

	"this annoucement can correspond to several kind of modifications:
		=> a method has been renamed
		=> a method has been move to another category // SHOULD BE TREATED WITH SystemMethodRecategorizedEvent
		(
			-maybe from the classic category to an extending package
			-maybe from an extending package to another extending package
			-maybe from an extending package to a classic category
			- maybe from a classic category to another classic category
			)
	"
	| oldMethod newMethod methodPackage |
	newMethod := ann newMethod.
	oldMethod := ann oldMethod.
	self flag: #cyrille. 
	"This is not clear that we need to do soemthing here since the method is removed and added so the other events should be handled correctly
	We should remove this method to see if this is working."
	
	"If the method origin is not the one of the event, we do not care about that method"
	newMethod origin = ann methodClass ifFalse: [^ self].
	
	
	"Special case for trait methods added an override"
	newMethod origin = oldMethod origin
		ifFalse: [ ^ self addMethod: newMethod ].
	
	methodPackage := newMethod packageFromOrganizer: self.
	"maybe the name of the method has changed"
	oldMethod selector = newMethod selector
		ifFalse: [ methodPackage updateSelector: oldMethod selector inClass: oldMethod methodClass withNewSelector: newMethod selector].
		

	