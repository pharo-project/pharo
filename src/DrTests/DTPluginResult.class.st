"
I am the abstract superclass for all classes that are results of a plugin in DrTests.
"
Class {
	#name : 'DTPluginResult',
	#superclass : 'Object',
	#category : 'DrTests-Model',
	#package : 'DrTests',
	#tag : 'Model'
}

{ #category : 'accessing' }
DTPluginResult >> backgroundColorStyle [

	^ 'testResult'
]

{ #category : 'accessing' }
DTPluginResult >> buildTreeForUI [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
DTPluginResult >> summarizeInfo [
	"Text showed in miniDrTests with info of the result "

	^ self asString
]

{ #category : 'accessing' }
DTPluginResult >> textColor [
	^ TestResult defaultColorText
]

{ #category : 'accessing' }
DTPluginResult >> theme [
	^ Smalltalk ui theme
]
