"
I'm a morph that shows the source codes of a compiled method 
"
Class {
	#name : #ReflectivityCodeMorph,
	#superclass : #Morph,
	#instVars : [
		'textModel',
		'compiledMethod'
	],
	#category : #'Reflectivity-Examples'
}

{ #category : #'instance creation' }
ReflectivityCodeMorph class >> compiledMethod: aCompiledMethod [

	^self new addSourceCode: aCompiledMethod; yourself.
]

{ #category : #accessing }
ReflectivityCodeMorph >> addSourceCode: aCompiledMethod [
	|editor|
	
	self compiledMethod: aCompiledMethod.
	
	editor := self newScrolledText.
	editor hResizing: #spaceFill; vResizing: #spaceFill.
	
	self textModel setText: aCompiledMethod sourceCode.
	self addMorph: editor.
]

{ #category : #accessing }
ReflectivityCodeMorph >> compiledMethod [
	^ compiledMethod
]

{ #category : #accessing }
ReflectivityCodeMorph >> compiledMethod: anObject [
	compiledMethod := anObject
]

{ #category : #initialize }
ReflectivityCodeMorph >> defaultBounds [
"answer the default bounds for the receiver"
	^ 0 @ 0 corner: 200 @ 40
]

{ #category : #initialize }
ReflectivityCodeMorph >> defaultColor [
	^Color veryLightGray
]

{ #category : #drawing }
ReflectivityCodeMorph >> drawOn: aCanvas [
	super drawOn: aCanvas.
	self drawTitleOn: aCanvas
]

{ #category : #drawing }
ReflectivityCodeMorph >> drawTitleOn: aCanvas [
	aCanvas drawString: self compiledMethod methodClass asString 
		in: (self bounds insetBy: (Margin left: 10 top: 10 right: 2 bottom: 10)) 
		font: (LogicalFont familyName: self familyName pointSize: 11)
		color: self theme textColor. 
]

{ #category : #accessing }
ReflectivityCodeMorph >> familyName [
	^'Source Sans Pro'
]

{ #category : #initialization }
ReflectivityCodeMorph >> initialize [

	super initialize.
	self
 		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		wrapDirection: #topToBottom;
		hResizing: #spaceFill;
		vResizing: #rigid;
		height: 150;
		layoutInset: (Margin top: 30 left: 2 bottom: 2 right: 2); 
		cellInset: 1.
		
]

{ #category : #'instance creation' }
ReflectivityCodeMorph >> newScrolledText [
	| scrolledText |
	scrolledText := self textModel
		newScrolledText
		beForSmalltalkCode;
		beWrapped;
		beReadOnly;
		yourself.
	^ scrolledText
]

{ #category : #shout }
ReflectivityCodeMorph >> shoutAboutToStyle: aTextView [

	aTextView classOrMetaClass: self compiledMethod methodClass.
	^ true
]

{ #category : #accessing }
ReflectivityCodeMorph >> textModel [
	^ textModel ifNil: [ textModel := RubScrolledTextModel new interactionModel: self ]
]
