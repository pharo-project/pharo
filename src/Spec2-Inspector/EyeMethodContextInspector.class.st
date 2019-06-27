"
I am an eye inspector specified for MethodContext
"
Class {
	#name : #EyeMethodContextInspector,
	#superclass : #EyeInspector,
	#category : #'Spec2-Inspector-Base'
}

{ #category : #accessing }
EyeMethodContextInspector class >> label [
	^ 'Context Inspector'
]

{ #category : #list }
EyeMethodContextInspector >> addVariableFields: elements [

	self tempNames withIndexDo: [ :name :index |
		elements add: (ContextTempEyeElement host: self object tempName: name tempIndex: index) ]
]

{ #category : #accessing }
EyeMethodContextInspector >> doItContext [
	^ self object
]

{ #category : #list }
EyeMethodContextInspector >> tempNames [

	^ [ self object tempNames ]
		on: Error
		do: [ #() ]
]
