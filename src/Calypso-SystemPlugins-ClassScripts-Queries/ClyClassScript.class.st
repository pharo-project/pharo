"
I am abstract class which represents kind of script which can be extracted and executed from class methods.
My subclasses define what methods are representing them. They should implement class side method #isImplementedByMethod:.

For example methods with pragma <sampleInstance> are represented by ClySampeInstanceScript

	ClySampeInstanceScript class >> isImplementedByMethod: aMethod
		^ aMethod hasPragmaNamed: #sampleInstance

In addition I define what kind of methods are able to provide scripts in general. By default it is always class side methods without arguments:

	ClyClassScript class >> canBeProvidedByMethod: aMethod
		^ aMethod origin isClassSide and: [ aMethod numArgs = 0 ]

If subclass define logic for instance side methods or for method arguments it should override method #canBeProvidedByMethod: in addition.
	
To create my instances use following method: 

	ClyClassScript createFrom: aMethod
 
To run the script send execute message: 

	aScript executeBy: aClass 
	
The argument can be different then the class which defines the method. Because the script can be run by subclasses.

I provide description methods for the UI. Some subclasses override them: 

- description 
- iconName 

Internal Representation and Key Implementation Points.

    Instance Variables
	implementorMethod:		<CompiledMethod>
"
Class {
	#name : #ClyClassScript,
	#superclass : #Object,
	#instVars : [
		'implementorMethod'
	],
	#category : #'Calypso-SystemPlugins-ClassScripts-Queries'
}

{ #category : #'script detection' }
ClyClassScript class >> canBeProvidedByMethod: aMethod [
	"This method just checks that method in general can be used for this kind of script"

	^ aMethod origin isClassSide and: [ aMethod numArgs = 0 ]
]

{ #category : #'instance creation' }
ClyClassScript class >> createFrom: aMethod [
	^self new 
		implementorMethod: aMethod
]

{ #category : #'script detection' }
ClyClassScript class >> detectInMethod: aMethod ifAbsent: absentBlock [
	

	self allSubclassesDo: [ :each | 
		((each canBeProvidedByMethod: aMethod)
			and: [each isImplementedByMethod: aMethod]) 
				ifTrue: [ ^ each createFrom: aMethod ]].
	^ absentBlock value
]

{ #category : #'script detection' }
ClyClassScript class >> isImplementedByMethod: aMethod [
	"Subclasses should override it.
	The method is not abstract to not corrupt browser if new script is not completelly implemented"
	
	^ false
]

{ #category : #accessing }
ClyClassScript >> description [
	^ 'Run the script'
]

{ #category : #execution }
ClyClassScript >> executeOn: aClass [
	"Execute the corresponding method of the argument."
	
	^ aClass instanceSide perform: implementorMethod selector
]

{ #category : #accessing }
ClyClassScript >> iconName [
	^ #scriptManagerIcon
]

{ #category : #accessing }
ClyClassScript >> implementorClass [
	^ implementorMethod origin 
]

{ #category : #accessing }
ClyClassScript >> implementorMethod [
	^ implementorMethod
]

{ #category : #accessing }
ClyClassScript >> implementorMethod: anObject [
	implementorMethod := anObject
]

{ #category : #accessing }
ClyClassScript >> implementorSelector [
	^ implementorMethod selector
]

{ #category : #testing }
ClyClassScript >> isImplementedByClass: aClass [
	
	^ aClass instanceSide respondsTo: self implementorSelector
]
