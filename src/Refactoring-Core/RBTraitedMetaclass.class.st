"
I represent the class side of a trait in a refactoring namespace. 

I shouldn't be created directly, but always be part of a refactoring namespace.
My  namespace usally knows me and my non meta class.
"
Class {
	#name : #RBTraitedMetaclass,
	#superclass : #RBMetaclass,
	#instVars : [
		'localMethods',
		'composition',
		'baseLocalMethods',
		'baseComposition'
	],
	#category : #'Refactoring-Core-Model'
}
