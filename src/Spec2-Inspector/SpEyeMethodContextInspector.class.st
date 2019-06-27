"
I am an eye inspector specified for MethodContext
"
Class {
	#name : #SpEyeMethodContextInspector,
	#superclass : #SpEyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
SpEyeMethodContextInspector class >> label [
	^ 'Context Inspector'
]

{ #category : #list }
SpEyeMethodContextInspector >> addVariableFields: elements [

	self tempNames withIndexDo: [ :name :index |
		elements add: (SpContextTempEyeElement host: self object tempName: name tempIndex: index) ]
]

{ #category : #accessing }
SpEyeMethodContextInspector >> doItContext [
	^ self object
]

{ #category : #list }
SpEyeMethodContextInspector >> tempNames [

	^ [ self object tempNames ]
		on: Error
		do: [ #() ]
]
