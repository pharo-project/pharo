"
I am an abstract class to represent a text attribute, the way a portion of text is graphically represented.
My sub-classes specify these representations:
- colouring (TextColor), 
- bold, italic, underlined, 
- narrow, strike out (TextEmphasis), 
- alignment (TextAlignment),
- indentation (TextIndent),
- font change (TextFontChange),
- kerning (TextKern),
- url and related action (TextAction hierarchy).
- morph (TextAnchor)

My instances are stored in the Text instance runs variable (a RunArray).

------------8<-----------------------------------------------------------------------------------------
Select text, press Command-6, choose a attribute.  If selected text is of the form 
	Hi There<Smalltalk beep>
the part in angle brackets is saved for action, and the Hi There appears in the paragraph.  If selection has no angle brackets, use the whole thing as both the text and the action.

TextDoIt  --  eval as a Smalltalk expression (the part in angle brackets)

TextLink -- Show a method, class comment, class hierarchy, or class defintion.
	<Point extent:>, <Point Comment>, <Point Hierarchy>, or <Point Defintion> are what you type.

TextURL -- Show the web page. <www.disney.com>

These attributes of text need to be stored on the disk in a regular file-out.  It is done in this form: 	Hi There   
	in the text, and a Run containing   dSmalltalk beep;;
	Click here to see the extent:   
	in the text, and a Run containing   method LPoint extent:;
See RunArray class scanFrom: where decoding is done.

"
Class {
	#name : #TextAttribute,
	#superclass : #Object,
	#category : #'Text-Core-Attributes'
}

{ #category : #accessing }
TextAttribute >> anchoredMorph [
	"If one hides here, return it"
	^nil
]

{ #category : #testing }
TextAttribute >> dominatedByCmd0 [
	"Subclasses may override if cmd-0 should turn them off"
	^ false
]

{ #category : #accessing }
TextAttribute >> dominates: another [
	"Subclasses may override condense multiple attributes"
	^ false
]

{ #category : #styling }
TextAttribute >> emphasisCode [
	"Subclasses may override to add bold, italic, etc"
	^ 0
]

{ #category : #styling }
TextAttribute >> forFontInStyle: aTextStyle do: aBlock [
	"No action is the default.  Overridden by font specs"
]

{ #category : #testing }
TextAttribute >> isKern [
	^false
]

{ #category : #testing }
TextAttribute >> mayActOnEvent: anEvent [
	"Subclasses may override to provide, eg, hot-spot actions"
	^ false
]

{ #category : #testing }
TextAttribute >> mayBeExtended [
	"A quality that may be overridden by subclasses, such as TextAnchors, that really only apply to a single character"
	^ true
]

{ #category : #testing }
TextAttribute >> mayHaveExternalReferences [
	"Answer if this attribute can have references to some external objects like AST nodes"
	^ false
]

{ #category : #initialization }
TextAttribute >> reset [
	"Allow subclasses to prepare themselves for merging attributes"
]

{ #category : #accessing }
TextAttribute >> set [
	"Respond true to include this attribute (as opposed to, eg, a bold
	emphasizer that is clearing the property"
	^ true
]
