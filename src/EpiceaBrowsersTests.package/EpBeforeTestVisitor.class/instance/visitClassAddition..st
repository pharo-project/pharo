visitor
visitClassAddition: aChange
	| utilClass |
	utilClass := classFactory newClassUsing: EpTCodeChangeVisitor.
	aChange initializeClass: utilClass.
	^ aChange