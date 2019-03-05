"
Description
--------------------

I am a demo on layout spacers
"
Class {
	#name : #SpecDemoSpacerPage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Layouts'
}

{ #category : #specs }
SpecDemoSpacerPage class >> pageName [
	^ 'Spacers in layout'
]

{ #category : #specs }
SpecDemoSpacerPage class >> priority [

	^ 50
]

{ #category : #initialization }
SpecDemoSpacerPage >> pageClass [

	^ SpecSpacerDemo
]
