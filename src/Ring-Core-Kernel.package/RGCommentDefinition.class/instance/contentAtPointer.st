source pointers
contentAtPointer
	"A RGCommentDefinition may be created to point the sourceFile in which case it retrieves the class comment"

	^ self sourcePointer notNil
		ifTrue: [ SourceFiles sourceCodeAt: self sourcePointer ]
		ifFalse:[ '' ]