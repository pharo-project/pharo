Class {
	#name : #CustomHelpMicrodownFormatter,
	#superclass : #Model,
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #formatting }
CustomHelpMicrodownFormatter >> bold: aString [
	"Return microdown  with bold emphasis."
	aString ifEmpty: [ ^ '' ].
	^ '**',aString,'**'

]

{ #category : #formatting }
CustomHelpMicrodownFormatter >> doItFrom: aString [
	"Return Text object with a specific color and mouse click action
	that executes aString; aString should also be a Smalltalk script that is possible to evaluate."
	
	^self doItFrom: aString text: aString

]

{ #category : #formatting }
CustomHelpMicrodownFormatter >> doItFrom: aString text: textString [
	"Return Text object with a specific color and mouse click action displayed as textString; 
	 aString should be a Smalltalk script that is possible to evaluate."
	^ '[',textString ,'](doit://', aString urlEncoded,')'

]

{ #category : #formatting }
CustomHelpMicrodownFormatter >> heading: aString [
	"Return Text object with heading formating attributes."
	^ String cr, '# ', aString, String cr
]

{ #category : #formatting }
CustomHelpMicrodownFormatter >> heading: aString withSubtitle: aSubtitleString [
	^ (self heading: aString), (self subheading: aSubtitleString )
]

{ #category : #accessing }
CustomHelpMicrodownFormatter >> sourceCode: aSourceCode [

	^'```
{1}
````' format: { aSourceCode  }

]

{ #category : #formatting }
CustomHelpMicrodownFormatter >> subheading: aString [
	"Return Text object with subheading formating attributes."
	^ String cr, '## ' , aString, Character cr asString
]

{ #category : #formatting }
CustomHelpMicrodownFormatter >> subsubheading: aString [
	^ String cr, '### ' , aString, String cr
]

{ #category : #formatting }
CustomHelpMicrodownFormatter >> underlined: aString [
	"Microdown do not do underline - should. make italic instead"
	^ '_',aString,'_'
]

{ #category : #formatting }
CustomHelpMicrodownFormatter >> url: aString [
	"Return Text object with copy aString to the clipboard after mouse click."
	
	^ '[{1}]({1})' format: { aString  }
	

]
