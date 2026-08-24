Class {
	#name : 'MyClassParent',
	#superclass : 'Object',
	#instVars : [
		'SharedVar'
	],
	#category : 'Refactoring-UI-Example',
	#package : 'Refactoring-UI',
	#tag : 'Example'
}

{ #category : 'accessing' }
MyClassParent >> method1 [

	MyClassChild new
]
