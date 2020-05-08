"
I add ""wrapp by scrollbars"" functionality to add to presenters that can be handled with or without them (like Lists, Tables and Texts areas)

Some backends like Morphic forces the scrollbars in their Tables and Lists. 
But some others like Gtk3 don't. This option allows you to configure the precence 
of scrollbars for platforms who do not have them automatically.
By default lists/tables will have scrollbars added by default, but you can tune this 
if your backend of choice supports it.
THIS CANNOT BE CHANGED ONCE THE WIDGET IS CREATED.
"
Trait {
	#name : #SpTHaveWrappingScrollBars,
	#instVars : [
		'wrapScrollBars',
		'propagateNaturalHeight',
		'propagateNaturalWidth',
		'scrollBarStyles'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #accessing }
SpTHaveWrappingScrollBars >> addScrollBarStyle: aStyle [

	scrollBarStyles := scrollBarStyles copyWith: aStyle
]

{ #category : #testing }
SpTHaveWrappingScrollBars >> hasScrollBars [
	"Some backends like Morphic forces the scrollbars in their Tables and Lists. 
	 But some others like Gtk3 don't. This option allows you to configure the precence 
	 of scrollbars for platforms who do not have them automatically.
	 By default lists/tables will have scrollbars added by default, but you can tune this 
	 if your backend of choice supports it.
	 THIS CANNOT BE CHANGED ONCE THE WIDGET IS CREATED."

	^ wrapScrollBars
]

{ #category : #initialization }
SpTHaveWrappingScrollBars >> initializeTHaveWrappingScrollBars [

	scrollBarStyles := #().
	self propagateNaturalWidth: false.
	self propagateNaturalHeight: false
]

{ #category : #testing }
SpTHaveWrappingScrollBars >> isPropagateNaturalHeight [

	^ propagateNaturalHeight ifNil: [ false ]
]

{ #category : #testing }
SpTHaveWrappingScrollBars >> isPropagateNaturalWidth [

	^ propagateNaturalWidth ifNil: [ false ]
]

{ #category : #accessing }
SpTHaveWrappingScrollBars >> propagateNaturalHeight: aBoolean [
	"Natural height propagation means that the height of the text presenter will follow the 
	 height of the text contained"

	propagateNaturalHeight := aBoolean
]

{ #category : #accessing }
SpTHaveWrappingScrollBars >> propagateNaturalWidth: aBoolean [
	"Natural width propagation means that the width of the text presenter will follow the 
	 width of the text contained. 
	 WARNING: Because of a limitation on how the font rendering works in morphic backend, 
	 this properly will fail to preserve the correct width if using monospaced fonts."

	propagateNaturalWidth := aBoolean
]

{ #category : #accessing }
SpTHaveWrappingScrollBars >> removeScrollBarStyle: aStyle [

	scrollBarStyles := scrollBarStyles copyWithout: aStyle
]

{ #category : #accessing }
SpTHaveWrappingScrollBars >> scrollBarStyles [

	^ scrollBarStyles ifNil: [ #() ]
]

{ #category : #accessing }
SpTHaveWrappingScrollBars >> withScrollBars [
	"Some backends like Morphic forces the scrollbars in their Tables and Lists. 
	 But some others like Gtk3 don't. This option allows you to configure the precence 
	 of scrollbars for platforms who do not have them automatically.
	 By default lists/tables will have scrollbars added by default, but you can tune this 
	 if your backend of choice supports it.
	 THIS CANNOT BE CHANGED ONCE THE WIDGET IS CREATED."

	wrapScrollBars := true
]

{ #category : #accessing }
SpTHaveWrappingScrollBars >> withoutScrollBars [
	"Some backends like Morphic forces the scrollbars in their Tables and Lists. 
	 But some others like Gtk3 don't. This option allows you to configure the precence 
	 of scrollbars for platforms who do not have them automatically.
	 By default lists/tables will have scrollbars added by default, but you can tune this 
	 if your backend of choice supports it.
	 THIS CANNOT BE CHANGED ONCE THE WIDGET IS CREATED."

	wrapScrollBars := false
]
