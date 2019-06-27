"
A simple editor for complied methods.
"
Class {
	#name : #EyeMethodEditor,
	#superclass : #EyeEditor,
	#instVars : [
		'toolbar'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #spec }
EyeMethodEditor class >> inspectorSpec [
	<spec>
	
	^ SpLayout composed
		add: #text;
		yourself
]

{ #category : #accessing }
EyeMethodEditor class >> label [
	^ 'Method Editor'
]

{ #category : #accessing }
EyeMethodEditor >> method [
	^ self object
]

{ #category : #'event-handling' }
EyeMethodEditor >> objectChanged [
	self text 
		text: self method sourceCode;
		behavior: self method methodClass.
	
	self toolbar method: self method
]

{ #category : #accessing }
EyeMethodEditor >> title [
	^ self method printString 
]

{ #category : #accessing }
EyeMethodEditor >> toolbar [
	^ toolbar ifNil: [ 
		toolbar := self instantiate: SpMethodToolbar. ]
]
