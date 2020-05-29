Class {
	#name : #SpLabelledTextInputTest,
	#superclass : #SpTextInputFieldPresenterTest,
	#category : #'Spec2-Tests-Core-Widgets'
}

{ #category : #running }
SpLabelledTextInputTest >> classToTest [
	^ SpLabelledTextInput
]

{ #category : #private }
SpLabelledTextInputTest >> textInputAdapter [

	^ presenter input adapter
]
