testing
isAbstract
	(self whichSelectorsReferToSymbol: #subclassResponsibility) isEmpty 
		ifFalse: [^true].
	model allReferencesToClass: self do: [:each | ^false].
	^true