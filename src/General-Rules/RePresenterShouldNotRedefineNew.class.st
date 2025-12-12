"
This rule warn the user if `new` method is redefined in a StPresenter subclass. 
Presenters should not define `new` again since they inherit from StPresenter `new` method.
"
Class {
	#name : 'RePresenterShouldNotRedefineNew',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
RePresenterShouldNotRedefineNew class >> ruleName [ 

	^ 'A presenter should not redefine "new" method'

]

{ #category : 'accessing' }
RePresenterShouldNotRedefineNew class >> severity [

	^ #warning
]

{ #category : 'running' }
RePresenterShouldNotRedefineNew >> basicCheck: aNode [

	| class |
	aNode isMethod ifFalse: [ ^ false ].
	aNode methodNode selector = #new ifFalse: [ ^ false ].
	class := aNode methodClass instanceSide.
	^ class inheritsFrom: StPresenter 
]
