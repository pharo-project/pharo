"
I am the abstract superclass for all classes that are results of a plugin in DrTests.
"
Class {
	#name : #DTPluginResult,
	#superclass : #Object,
	#category : #'DrTests-Model'
}

{ #category : #accessing }
DTPluginResult >> backgroundColor [
	^ Color named: 'gray'
]

{ #category : #accessing }
DTPluginResult >> buildTreeForUI [
	^ self subclassResponsibility
]

{ #category : #accessing }
DTPluginResult >> summarizeInfo [
	"Text showed in miniDrTests with info of the result "

	^ self asString
]

{ #category : #accessing }
DTPluginResult >> textColor [
	^ TestResult defaultColorText
]

{ #category : #accessing }
DTPluginResult >> theme [
	^ Smalltalk ui theme
]
