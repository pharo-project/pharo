"
I add ""wrapp by scrollbars"" functionality to add to presenters that can be handled with or without them (like Lists, Tables and Texts areas)

Some backends like Morphic forces the scrollbars in their Tables and Lists. 
But some others like Gtk3 don't. This traits adds configuration of the precence 
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

{ #category : #api }
SpTHaveWrappingScrollBars >> addScrollBarStyle: aStyle [
	"Add a style for the scrollbar component. 
	 In platforms that do not support separation from component and scrollbar (like Morphic), the 
	 style will be added to the component styles. 
	 In platforms who supports separation between component and scrollbar (like Gtk3), the style 
	 will be taken separately."

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
	"Indicate if component propagates natural heigth.
	 See `SpTHaveWrappingScrollBars>>#propagateNaturalHeight:`"

	^ propagateNaturalHeight ifNil: [ false ]
]

{ #category : #testing }
SpTHaveWrappingScrollBars >> isPropagateNaturalWidth [
	"Indicate if component propagates natural width.
	 See `SpTHaveWrappingScrollBars>>#propagateNaturalWidth:`"

	^ propagateNaturalWidth ifNil: [ false ]
]

{ #category : #api }
SpTHaveWrappingScrollBars >> propagateNaturalHeight: aBoolean [
	"Indicate when component propagates natural heigth.
	 Natural height propagation means that the height of the presenter containing both scrollbars and 
	 another component (tipically a text presenter) will follow the height of such component."

	propagateNaturalHeight := aBoolean
]

{ #category : #api }
SpTHaveWrappingScrollBars >> propagateNaturalWidth: aBoolean [
	"Indicate when component propagates natural width.
	 Natural width propagation means that the width of the presenter containing both scrollbars and 
	 another component (tipically a text presenter) will follow the width of such component.
	 WARNING: Because of a limitation on how the font rendering works in morphic backend, 
	 this properly will fail to preserve the correct width if using monospaced fonts."

	propagateNaturalWidth := aBoolean
]

{ #category : #api }
SpTHaveWrappingScrollBars >> removeScrollBarStyle: aStyle [

	scrollBarStyles := scrollBarStyles copyWithout: aStyle
]

{ #category : #api }
SpTHaveWrappingScrollBars >> scrollBarStyles [
	"Answer styles defined for the scrollbar component. 
	 In platforms that do not support separation from component and scrollbar (like Morphic), the 
	 style will be added to the component styles. 
	 In platforms who supports separation between component and scrollbar (like Gtk3), the style 
	 will be taken separately."

	^ scrollBarStyles ifNil: [ #() ]
]

{ #category : #api }
SpTHaveWrappingScrollBars >> withScrollBars [
	"Some backends like Morphic forces the scrollbars in their Tables and Lists. 
	 But some others like Gtk3 don't. This option allows you to configure the precence 
	 of scrollbars for platforms who do not have them automatically.
	 By default lists/tables will have scrollbars added by default, but you can tune this 
	 if your backend of choice supports it.
	 THIS CANNOT BE CHANGED ONCE THE WIDGET IS CREATED."

	wrapScrollBars := true
]

{ #category : #api }
SpTHaveWrappingScrollBars >> withoutScrollBars [
	"Some backends like Morphic forces the scrollbars in their Tables and Lists. 
	 But some others like Gtk3 don't. This option allows you to configure the precence 
	 of scrollbars for platforms who do not have them automatically.
	 By default lists/tables will have scrollbars added by default, but you can tune this 
	 if your backend of choice supports it.
	 THIS CANNOT BE CHANGED ONCE THE WIDGET IS CREATED."

	wrapScrollBars := false
]
