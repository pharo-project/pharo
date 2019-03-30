"
My strategy is to raise an error if the class doesn't exist
"
Class {
	#name : #ClassResolverStrictResolve,
	#superclass : #ClassResolver,
	#category : #'Shift-ClassBuilder'
}

{ #category : #resolving }
ClassResolverStrictResolve >> resolve: aClassName [
	"^Error new:'You''re superclass doesn''t exist'."
	^ self error: 'Superclass ', aClassName,' doesn''t exist'
]
