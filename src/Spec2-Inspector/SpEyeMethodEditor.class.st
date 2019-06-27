"
A simple editor for complied methods.
"
Class {
	#name : #SpEyeMethodEditor,
	#superclass : #SpEyeEditor,
	#instVars : [
		'toolbar'
	],
	#category : #'Spec2-Inspector-Base'
}

{ #category : #spec }
SpEyeMethodEditor class >> inspectorSpec [
	<spec>
	
	^ SpLayout composed
		add: #text;
		yourself
]

{ #category : #accessing }
SpEyeMethodEditor class >> label [
	^ 'Method Editor'
]

{ #category : #accessing }
SpEyeMethodEditor >> method [
	^ self object
]

{ #category : #'event-handling' }
SpEyeMethodEditor >> objectChanged [
	self text 
		text: self method sourceCode;
		behavior: self method methodClass.
	
	self toolbar method: self method
]

{ #category : #accessing }
SpEyeMethodEditor >> title [
	^ self method printString 
]

{ #category : #accessing }
SpEyeMethodEditor >> toolbar [
	^ toolbar ifNil: [ 
		toolbar := self instantiate: SpMethodToolbar. ]
]
