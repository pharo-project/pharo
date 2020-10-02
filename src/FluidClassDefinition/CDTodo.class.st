Class {
	#name : #CDTodo,
	#superclass : #Object,
	#category : #'FluidClassDefinition-Visitor'
}

{ #category : #'as yet unclassified' }
CDTodo >> todo2 [

	"in classDefinitionNode className should be name"
]

{ #category : #'as yet unclassified' }
CDTodo >> todo3 [

	"in ClassDefinition node traitdefnition is traitComposition "
]

{ #category : #'as yet unclassified' }
CDTodo >> todo4 [

	"class side parsing 
		what happens if we have traits at the class side and slots
	
	AlignmentMorph class
		uses: TableRotate classTrait;
		slots: { #x . #y}
	
	Same question for traits.
		Tassertable classTrait
			uses: ....
	
	"
]

{ #category : #'as yet unclassified' }
CDTodo >> todo5 [

	"Expandable definition for metaclass, traits + verification they work."
]

{ #category : #'as yet unclassified' }
CDTodo >> todo6 [

	"Expandable definition should work also on old class definition"
]
