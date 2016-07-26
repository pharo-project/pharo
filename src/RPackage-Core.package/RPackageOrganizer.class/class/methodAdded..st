private
methodAdded: anEvent
	"precondition: package exist, class exist"
	
	| methodCategory |
	methodCategory := anEvent protocol.
	(methodCategory isEmptyOrNil or:[ methodCategory first ~= $* ]) 
		ifFalse: [
			(self isPackageDefinedForClass: anEvent methodClass)
					ifFalse: [self packageClass new named: (Smalltalk organization categoryOfElement: anEvent methodClass theNonMetaClass asSymbol) ].
		].