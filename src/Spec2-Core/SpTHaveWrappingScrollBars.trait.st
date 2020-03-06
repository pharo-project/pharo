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
		'wrapScrollBars'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

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
