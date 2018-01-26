"
Abstract superclass for SUnit tests for smart suggestions classes
"
Class {
	#name : #SugsSuggestionTest,
	#superclass : #TestCase,
	#instVars : [
		'emptyClass'
	],
	#category : #'SmartSuggestions-Tests-Tests'
}

{ #category : #testing }
SugsSuggestionTest class >> isAbstract [

	^self name = #SugsSuggestionTest
]

{ #category : #private }
SugsSuggestionTest >> addMethodToDelegate [
	self emptyClass compile: 'methodToDelegate<n><t>2+5. <n><t>OrderedCollection new.' expandMacros classified: #testing
]

{ #category : #private }
SugsSuggestionTest >> addVariable: name to: aClass [
	name first isUppercase ifTrue: [ aClass addClassVarNamed: name] ifFalse: [aClass addInstVarNamed: name].
]

{ #category : #private }
SugsSuggestionTest >> createAccessorsFor: varName in: aClass [

	self createGetter: varName in: aClass.	
	self createSetter: varName in: aClass .
	
	
]

{ #category : #private }
SugsSuggestionTest >> createClass: symbol [
	| newClass |
	
	newClass := Object 
					subclass: symbol
					instanceVariableNames: ''
					classVariableNames: ''
					poolDictionaries: ''
					category: 'SmartSuggestions-Test'.
	^newClass.
]

{ #category : #private }
SugsSuggestionTest >> createContext: selectedVariableName [
	^ SugsMockContext for: self emptyClass selectedVariable: selectedVariableName 
]

{ #category : #private }
SugsSuggestionTest >> createGetter: varName in: aClass [
	| target name |

	target := self targetFor: varName in: aClass.
	name := self nameFor: varName.	
	target
		compile: ('<1s><n><t>^ <2s>' expandMacrosWith: name with: varName)
		classified: #accessing.
	
	
]

{ #category : #private }
SugsSuggestionTest >> createSetter: varName in: aClass [ 
	| target name |
	
	target := self targetFor: varName in: aClass.
	name := self nameFor: varName.	
	target
		compile: ('<1s> anObject<n><t><2s> := anObject' expandMacrosWith: (name,':') with: varName)
		classified: #accessing.
	
	
]

{ #category : #private }
SugsSuggestionTest >> emptyClass [
	^ emptyClass
]

{ #category : #private }
SugsSuggestionTest >> nameFor: varName [
	| name |
	
	name:= varName copy.
	name at: 1 put: name first asLowercase.
	^name.
]

{ #category : #private }
SugsSuggestionTest >> removeClass: aClass [
	aClass removeFromSystem.
]

{ #category : #running }
SugsSuggestionTest >> setUp [
	super setUp.
	emptyClass  := self createClass: #TestWithAccessors.
]

{ #category : #running }
SugsSuggestionTest >> tearDown [
	
	self removeClass: emptyClass.
	super tearDown
]
