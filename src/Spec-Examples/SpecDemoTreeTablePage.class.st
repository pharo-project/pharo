"
Description
--------------------

I am a demo showing a demo of tree tables.
"
Class {
	#name : #SpecDemoTreeTablePage,
	#superclass : #SpecDemoPage,
	#category : #'Spec-Examples-Demo-Tables'
}

{ #category : #initialization }
SpecDemoTreeTablePage class >> pageName [
	^ 'Tree tables'
]

{ #category : #initialization }
SpecDemoTreeTablePage >> pageClass [
	^ SpecDemoTreeTablePresenter
]
