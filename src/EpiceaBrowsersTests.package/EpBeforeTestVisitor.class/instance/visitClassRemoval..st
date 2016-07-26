visitor
visitClassRemoval: aChange
	| utilClass |
	utilClass := classFactory newClassUsing: EpTCodeChangeVisitor.
	miniMonticello snapshot.
	aChange initializeClassRemoved: utilClass.
	classFactory delete: utilClass.
	^ aChange