visitor
visitClassModification: aChange
	| aClass aClassOld |
	aClass := classFactory newClass.
	miniMonticello snapshot.
	aClassOld := aClass copy.
	aClass := aClass superclass
		subclass: aClass name asSymbol
		uses: EpTCodeChangeVisitor
		instanceVariableNames: 'yaddaYadda'
		classVariableNames: ''
		category: aClass category.
	aChange initializeOldClass: aClassOld newClass: aClass.
	^ aChange.