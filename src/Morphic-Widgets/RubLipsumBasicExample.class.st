"
""When the text is wrapped, it is slower but the text has 1798 lines""
[self openWrapped] timeToRun  
     
""Enjoy the speed when not wrapped! ""
[self openNotWrapped] timeToRun 
 
"
Class {
	#name : 'RubLipsumBasicExample',
	#superclass : 'Model',
	#instVars : [
		'textModel'
	],
	#category : 'Morphic-Widgets-Editing-Widgets-Examples',
	#package : 'Morphic-Widgets',
	#tag : 'Editing-Widgets-Examples'
}

{ #category : 'examples' }
RubLipsumBasicExample class >> openNotWrapped [

	<example>
	^ self openNotWrappedIn: self currentWorld
]

{ #category : 'opening' }
RubLipsumBasicExample class >> openNotWrappedIn: aWorld [

	^ self openScrolledText: self new newScrolledTextNotWrapped in: aWorld
]

{ #category : 'examples' }
RubLipsumBasicExample class >> openScrolledText: aScrolledText [

	^ self openScrolledText: aScrolledText in: self currentWorld
]

{ #category : 'opening' }
RubLipsumBasicExample class >> openScrolledText: aScrolledText in: aWorld [

	| window |
	window := StandardWindow new.
	window addMorph: aScrolledText fullFrame: LayoutFrame identity.
	window title: self title.
	^ window openInWorld: aWorld
]

{ #category : 'examples' }
RubLipsumBasicExample class >> openWrapped [

	<example>
	^ self openWrappedIn: self currentWorld
]

{ #category : 'opening' }
RubLipsumBasicExample class >> openWrappedIn: aWorld [

	^ self openScrolledText: self new newScrolledTextWrapped in: aWorld
]

{ #category : 'examples' }
RubLipsumBasicExample class >> title [
	^ 'Lorem Ipsum example'
]

{ #category : 'accessing' }
RubLipsumBasicExample >> loremIpsum [
	^ String loremIpsum: 81233
]

{ #category : 'user interface' }
RubLipsumBasicExample >> newScrolledText [
	| st |
	st := self textModel newScrolledText
		withAdornment;
		withLineNumbers;
		withColumns;
		withGhostText: 'Hello, the text is now empty. Don''t worry, I will disappear if you enter some text.';
		withAnnotation;
		yourself.
	st ghostTextRuler fontName: 'Bitmap DejaVu Sans' pointSize: 18; centered.
	^ st
]

{ #category : 'user interface' }
RubLipsumBasicExample >> newScrolledTextNotWrapped [
	^ self newScrolledText
]

{ #category : 'user interface' }
RubLipsumBasicExample >> newScrolledTextWrapped [
	^ self newScrolledText beWrapped
]

{ #category : 'accessing' }
RubLipsumBasicExample >> textModel [

	^ textModel ifNil: [
			  textModel := RubScrolledTextModel new
				               setText: self loremIpsum;
				               yourself ]
]
