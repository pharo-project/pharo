"
I open a HelpBrowser for the currently active debugger.

To specify a help topic a debugger has to override the class side method #helpClass.
"
Class {
	#name : #GTHelpDebugAction,
	#superclass : #DebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTHelpDebugAction class >> gtDebuggingAction [
	"<gtDebuggingAction>"
]

{ #category : #accessing }
GTHelpDebugAction >> defaultIcon [

	^ GLMUIThemeExtraIcons glamorousHelp
]

{ #category : #accessing }
GTHelpDebugAction >> defaultLabel [

	^ ''
]

{ #category : #accessing }
GTHelpDebugAction >> defaultOrder [

	^ 200
]

{ #category : #actions }
GTHelpDebugAction >> executeAction [

	HelpBrowser open selectTopicSatisfying: [:each | 
		each owner notNil and:  [
			(each owner key = self debugger class helpClass key) and: [
				each title = 'Overview' ] ] ]
]

{ #category : #accessing }
GTHelpDebugAction >> help [
	^ 'Open an help browser for the currently active debugger.'
]

{ #category : #initialization }
GTHelpDebugAction >> initialize [
	super initialize.
	self needsUpdate: false
]
