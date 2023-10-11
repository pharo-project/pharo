"
My strategy is to raise an error if the class doesn't exist
"
Class {
	#name : 'ClassResolverStrictResolve',
	#superclass : 'ClassResolver',
	#category : 'Shift-ClassBuilder',
	#package : 'Shift-ClassBuilder'
}

{ #category : 'resolving' }
ClassResolverStrictResolve >> resolve: aClassName [

	^ self error: 'Superclass ', aClassName,' doesn''t exist'
]
