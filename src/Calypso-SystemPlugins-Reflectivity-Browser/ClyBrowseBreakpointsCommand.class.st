"
I am a command to open browser with methods where halts or breakpoints are installed.

I am registered in world toolbar using class side method #worldMenuOn:
"
Class {
	#name : 'ClyBrowseBreakpointsCommand',
	#superclass : 'ClyCommand',
	#category : 'Calypso-SystemPlugins-Reflectivity-Browser-Breakpoints',
	#package : 'Calypso-SystemPlugins-Reflectivity-Browser',
	#tag : 'Breakpoints'
}

{ #category : 'accessing' }
ClyBrowseBreakpointsCommand >> description [

	^'Open browser with methods where Halts or Breakpoints are installed'
]

{ #category : 'execution' }
ClyBrowseBreakpointsCommand >> execute [

	ClyQueryBrowserMorph browseBreakpoints
]

{ #category : 'accessing' }
ClyBrowseBreakpointsCommand >> iconName [

	^#smallDebug
]

{ #category : 'accessing' }
ClyBrowseBreakpointsCommand >> name [

	^'Browse Breakpoints'
]
