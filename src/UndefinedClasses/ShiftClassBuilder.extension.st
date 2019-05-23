Extension { #name : #ShiftClassBuilder }

{ #category : #'*UndefinedClasses' }
ShiftClassBuilder >> useUndefinedClass [
	"If i am use, the superResolver will resolve unknown superclass by creating an UndefinedClass with the expected superclassName"

	self superclassResolver: UndefinedClassClassResolver new
]
